package com.SpringChurchCRMSystem.SpringChurchCRMSystem.service;

import java.io.File;
import java.util.List;
import java.util.Optional;
import java.util.Random;
import java.util.concurrent.ConcurrentHashMap;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.User;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.UserRepository;

import jakarta.mail.MessagingException;
import jakarta.mail.internet.MimeMessage;
import jakarta.servlet.http.HttpSession;
import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class UserService {
    @Autowired
    private UserRepository userRepository;
    private final ConcurrentHashMap<String, String> otpCache = new ConcurrentHashMap<>();
    private final JavaMailSender emailDispatcher;
    private final HttpSession userSession;

    private BCryptPasswordEncoder encoder = new BCryptPasswordEncoder(12);

    // Creating a new User
    public String createUser(User user) {
        if (userRepository.findByEmail(user.getEmail()).isPresent()) {
            return "Email exits ";
        } else {
            user.setPassword(encoder.encode(user.getPassword()));
            userRepository.save(user);
            return "User Created Successfully";
        }
    }

    // Generating reset random numbers
    private String generateResetCode() {
        return String.valueOf(100000 + new Random().nextInt(900000));
    }

    // Generating login random numbers
    private String generateLoginCode() {
        return String.valueOf(100000 + new Random().nextInt(900000));
    }

    // Send Reset Password OTP

    public String sendPasswordResetOtp(String email) {
        Optional<User> userOptional = userRepository.findByEmail(email);
        if (userOptional.isEmpty()) {
            return " Email not found.";
        }

        String verifyCode = generateResetCode();
        otpCache.put(email + "reset", verifyCode);

        try {
            MimeMessage mimeMessage = emailDispatcher.createMimeMessage();
            MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true, "UTF-8");

            helper.setTo(email);
            helper.setSubject("🔐 Your ChurchCRM Reset Password Code ");

            String htmlBody = """
                    <html>
                        <body style="margin:0; padding:0; background-color:#f3f3f3; font-family:'Segoe UI', sans-serif;">
                            <div style="max-width:600px; margin:auto; background-color:#ffffff; border-radius:10px; padding:40px; box-shadow:0 4px 12px rgba(0,0,0,0.1); text-align:center;">

                                <!-- Logo -->
                                <img src='cid:churchLogo' alt='Church Logo' style='height:80px; margin-bottom:20px;'/>

                                <!-- Greeting -->
                                <h2 style="color:#4a148c; margin-bottom:10px;">Welcome! </h2>
                              <p style="color:#555; font-size:16px;">To reset your password account securely, please use the reset code below:</p>

                                <!-- OTP Code -->
                                <div style="margin:30px 0;">
                                    <div style="display:inline-block; background-color:#fff3cd; color:#d32f2f; font-size:36px; font-weight:bold; padding:15px 30px; border-radius:8px; border:1px solid #ffeeba;">
                                        %s
                                    </div>
                                    <p style="color:#777; font-size:14px; margin-top:10px;"> Please do not share it with anyone.</p>
                                </div>

                                <!-- Footer -->
                                <hr style="border:none; border-top:1px solid #eee; margin:30px 0;">
                                <div style="font-size:13px; color:#999;">
                                 <p>&copy; 2025 ChurchCRM System </p>
                                </div>
                            </div>
                        </body>
                    </html>
                    """
                    .formatted(verifyCode);

            helper.setText(htmlBody, true);

            // Embed logo image inline
            File imageFile = new File("src/main/resources/static/church-logo.png");
            helper.addInline("churchLogo", imageFile);

            emailDispatcher.send(mimeMessage);
            return "Reset OTP sent successfully to " + email;

        } catch (MessagingException e) {
            e.printStackTrace();
            return " Failed to send OTP email.";
        }
    }

    // verify forgot password otp and reset the password
    public String verifyResetCodeAndUpdatePassword(String email, String verificationCode, String newPassword) {
        String cacheKey = email + "reset";
        String storedCode = otpCache.get(cacheKey);
        if (storedCode == null || !storedCode.equals(verificationCode)) {
            return "Invalid Otp";
        }
        Optional<User> userOptional = userRepository.findByEmail(email);
        if (userOptional.isEmpty()) {
            return "Email not found";
        } else if (newPassword != null) {
            User user = userOptional.get();
            user.setPassword(encoder.encode(newPassword));
            userRepository.save(user);
            otpCache.remove(cacheKey);

            return "Password Changed Successfully";

        } else {
            return "Password not saved";
        }
    }

    // send login otp
    public String sendLoginOtp(String email) {
        Optional<User> userOptional = userRepository.findByEmail(email);
        if (userOptional.isEmpty()) {
            return "Email not found.";
        }

        String verifyCode = generateLoginCode();
        otpCache.put(email + "login", verifyCode);

        try {
            MimeMessage mimeMessage = emailDispatcher.createMimeMessage();
            MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true, "UTF-8");

            helper.setTo(email);
            helper.setSubject("🔐 Your ChurchCRM Login Code ");

            String htmlBody = """
                    <html>
                        <body style="margin:0; padding:0; background-color:#f3f3f3; font-family:'Segoe UI', sans-serif;">
                            <div style="max-width:600px; margin:auto; background-color:#ffffff; border-radius:10px; padding:40px; box-shadow:0 4px 12px rgba(0,0,0,0.1); text-align:center;">

                                <!-- Logo -->
                                <img src='cid:churchLogo' alt='Church Logo' style='height:80px; margin-bottom:20px;'/>

                                <!-- Greeting -->
                                <h2 style="color:#4a148c; margin-bottom:10px;">Welcome! </h2>
                              <p style="color:#555; font-size:16px;">To access your account securely, please use the login code below:</p>

                                <!-- OTP Code -->
                                <div style="margin:30px 0;">
                                    <div style="display:inline-block; background-color:#fff3cd; color:#d32f2f; font-size:36px; font-weight:bold; padding:15px 30px; border-radius:8px; border:1px solid #ffeeba;">
                                        %s
                                    </div>
                                    <p style="color:#777; font-size:14px; margin-top:10px;"> Please do not share it with anyone.</p>
                                </div>

                                <!-- Footer -->
                                <hr style="border:none; border-top:1px solid #eee; margin:30px 0;">
                                <div style="font-size:13px; color:#999;">
                                 <p>&copy; 2025 ChurchCRM System </p>
                                </div>
                            </div>
                        </body>
                    </html>
                    """
                    .formatted(verifyCode);

            helper.setText(htmlBody, true);

            // Embed logo image inline
            File imageFile = new File("src/main/resources/static/church-logo.png");
            helper.addInline("churchLogo", imageFile);

            emailDispatcher.send(mimeMessage);
            return "Login OTP sent successfully to " + email;

        } catch (MessagingException e) {
            e.printStackTrace();
            return " Failed to send OTP email.";
        }
    }

    // verify otp and login
    public User verifyLoginCodeAndLogin(String email, String verifyCode, String Password) {
        String cacheKey = email + "login";
        String storedCode = otpCache.get(cacheKey);
        if (storedCode == null || !storedCode.equals(verifyCode)) {
            throw new IllegalArgumentException("Invalid Otp");
        }
        Optional<User> userOptional = userRepository.findByEmail(email);
        if (userOptional.isEmpty()) {
            throw new IllegalArgumentException("Email not found");
        }
        User user = userOptional.get();
        if (!encoder.matches(Password, user.getPassword())) {
            throw new IllegalArgumentException("Incorrect Password");
        }

        otpCache.remove(cacheKey);
        userSession.setAttribute("loggedInUser", user);
        return user;

    }

    // Destroy the session

    public String destroySession() {
        userSession.removeAttribute("loggedInUser");
        userSession.invalidate();
        return "Session Terminated";
    }

    // Get all Users
    public List<User> getAllUsers() {
        return userRepository.findAll();
    }

    // Get all paginated users
    public Page<User> getPaginatedUsers(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size);
        return userRepository.findAll(pageable);
    }

    // delete user
    public String deleteUser(String userId) {
        if (!userRepository.existsById(userId)) {
            return "Id does not exists";
        }
        userRepository.deleteById(userId);
        return "User deleted succefully";
    }

    // update user
    public String updateUser(String userId, User user) {
        if (!userRepository.existsById(userId)) {
            return "Id does not exists";
        }
        user.setUserId(userId);
        userRepository.save(user);
        return "User saved succefully";
    }

}
