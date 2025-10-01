package com.SpringChurchCRMSystem.SpringChurchCRMSystem.service;

import java.io.File;

import java.util.List;
import java.util.Optional;
import java.util.Random;
import java.util.concurrent.ConcurrentHashMap;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import org.springframework.http.ResponseEntity;

import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

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

    // creating a user
    public ResponseEntity<String> createUser(User user, MultipartFile file) {
        try {
            if (userRepository.findByEmail(user.getEmail()).isPresent()) {
                return ResponseEntity.ok("Status 5000"); // Email exists
            }

            // Encode password
            user.setPassword(encoder.encode(user.getPassword()));

            // Handle profile picture
            if (file != null && !file.isEmpty()) {
                user.setProfilePic(file.getBytes());
            }

            // Save user with image
            userRepository.save(user);
            return ResponseEntity.ok("Status 1000"); // Success

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 9999"); // Unknown error
        }
    }

    // update a user
    public ResponseEntity<String> updateUser(String userId, User updatedData, MultipartFile file) {
        try {
            Optional<User> userOpt = userRepository.findById(userId);
            if (userOpt.isEmpty()) {
                return ResponseEntity.ok("Status 3000"); // User not found
            }

            User user = userOpt.get();

            // Update fields if provided
            if (updatedData.getNames() != null) {
                user.setNames(updatedData.getNames());
            }

            if (updatedData.getEmail() != null) {
                user.setEmail(updatedData.getEmail());
            }

            if (updatedData.getPhone() != null) {
                user.setPhone(updatedData.getPhone());
            }

            if (updatedData.getNationalId() != null) {
                user.setNationalId(updatedData.getNationalId());
            }

            if (updatedData.getRole() != null) {
                user.setRole(updatedData.getRole());
            }

            if (updatedData.getLevel() != null) {
                user.setLevel(updatedData.getLevel());
            }

            // Update profile picture if provided
            if (file != null && !file.isEmpty()) {
                user.setProfilePic(file.getBytes());
            }

            userRepository.save(user);
            return ResponseEntity.ok("Status 1000"); // Success

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 9999"); // Unknown error
        }
    }

    // Disabling a an active user
    public ResponseEntity<String> disableUser(String userId) {
        try {
            Optional<User> userOpt = userRepository.findById(userId);
            if (userOpt.isEmpty()) {
                return ResponseEntity.ok("Status 3000"); // User not found
            }

            User user = userOpt.get();

            if (!user.isActive()) {
                return ResponseEntity.ok("Status 3000"); // Already disabled
            }

            user.setActive(false);
            userRepository.save(user);

            return ResponseEntity.ok("Status 1000"); // Success

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 9999"); // Unknown error
        }
    }

    // Enabling an inactive user
    public ResponseEntity<String> enableUser(String userId) {
        try {
            Optional<User> userOpt = userRepository.findById(userId);
            if (userOpt.isEmpty()) {
                return ResponseEntity.ok("Status 3000"); // User not found
            }

            User user = userOpt.get();

            if (user.isActive()) {
                return ResponseEntity.ok("Status 3000"); // Already enabled
            }

            user.setActive(true);
            userRepository.save(user);

            return ResponseEntity.ok("Status 1000"); // Success

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 9999"); // Unknown error
        }
    }

    // Get all Users regardless of status
    public List<User> getAllUsers() {
        return userRepository.findAll();
    }

    // Get all active users
    public List<User> getAllActiveUsers() {
        return userRepository.findByIsActiveTrue();
    }

    // Get all inactive users
    public List<User> getAllInActiveUsers() {
        return userRepository.findByIsActiveFalse();
    }

    // Getting all paginated users
    public Page<User> getPaginatedUsers(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size);
        return userRepository.findAll(pageable);
    }

    // Getting all paginated Active users
    public Page<User> getPaginatedActiveUsers(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size);
        return userRepository.findByIsActiveTrue(pageable);
    }

    // Getting all paginated Inactive users
    public Page<User> getPaginatedInactiveUsers(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size);
        return userRepository.findByIsActiveFalse(pageable);
    }

    // Generating reset random numbers
    private String generateResetCode() {
        return String.valueOf(100000 + new Random().nextInt(900000));
    }

    // Generating login random numbers
    private String generateLoginCode() {
        return String.valueOf(100000 + new Random().nextInt(900000));
    }

    // send login otp

    public ResponseEntity<String> sendLoginOtp(String email, String password) {
        try {
            Optional<User> userOptional = userRepository.findByEmail(email);
            if (userOptional.isEmpty()) {
                return ResponseEntity.ok("Status 3000"); // Email not found
            }

            User user = userOptional.get();
            if (!encoder.matches(password, user.getPassword())) {
                return ResponseEntity.ok("Status 4000"); // Incorrect password
            }

            // Check user status
            if (!user.isActive()) {
                // User is disabled
                return ResponseEntity.ok("6000");
            }
            // Check level status
            if (user.getLevel() == null || !user.getLevel().isActive()) {
                // inactive level
                return ResponseEntity.ok("6000");
            }
            String verifyCode = generateLoginCode();
            otpCache.put(email + "login", verifyCode);

            MimeMessage mimeMessage = emailDispatcher.createMimeMessage();
            MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true, "UTF-8");

            helper.setTo(email);
            helper.setSubject("🔐 Your ChurchCRM Login Code");

            String htmlBody = """
                    <html>
                        <body style="margin:0; padding:0; background-color:#f3f3f3; font-family:'Segoe UI', sans-serif;">
                            <div style="max-width:600px; margin:auto; background-color:#ffffff; border-radius:10px; padding:40px; box-shadow:0 4px 12px rgba(0,0,0,0.1); text-align:center;">
                                <img src='cid:churchLogo' alt='Church Logo' style='height:80px; margin-bottom:20px;'/>
                                <h2 style="color:#4a148c; margin-bottom:10px;">Welcome!</h2>
                                <p style="color:#555; font-size:16px;">Use the login code below to access your account:</p>
                                <div style="margin:30px 0;">
                                    <div style="display:inline-block; background-color:#fff3cd; color:#d32f2f; font-size:36px; font-weight:bold; padding:15px 30px; border-radius:8px; border:1px solid #ffeeba;">
                                        %s
                                    </div>
                                    <p style="color:#777; font-size:14px; margin-top:10px;">Do not share this code.</p>
                                </div>
                                <hr style="border:none; border-top:1px solid #eee; margin:30px 0;">
                                <div style="font-size:13px; color:#999;">
                                    <p>&copy; 2025 ChurchCRM System</p>
                                </div>
                            </div>
                        </body>
                    </html>
                    """
                    .formatted(verifyCode);

            helper.setText(htmlBody, true);
            File imageFile = new File("src/main/resources/static/church-logo.png");
            helper.addInline("churchLogo", imageFile);

            emailDispatcher.send(mimeMessage);
            return ResponseEntity.ok("Status 1000"); // OTP sent successfully

        } catch (MessagingException e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 2000"); // Email sending failed
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 9999"); // Unknown error
        }
    }
    // verify otp and login

    public ResponseEntity<String> verifyLoginCodeAndLogin(String email, String verifyCode, String password) {
        String cacheKey = email + "login";
        String storedCode = otpCache.get(cacheKey);

        try {
            if (storedCode == null || !storedCode.equals(verifyCode)) {
                return ResponseEntity.ok("Status 3000"); // Invalid OTP
            }

            Optional<User> userOptional = userRepository.findByEmail(email);
            if (userOptional.isEmpty()) {
                return ResponseEntity.ok("Status 3000"); // Email not found
            }

            User user = userOptional.get();

            if (!encoder.matches(password, user.getPassword())) {
                return ResponseEntity.ok("Status 4000"); // Incorrect password
            }

            otpCache.remove(cacheKey);
            userSession.setAttribute("loggedInUser", user);
            return ResponseEntity.ok("Status 1000"); // Login successful

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 9999"); // Unknown error
        }
    }

    // Send Reset Password OTP

    public ResponseEntity<String> sendPasswordResetOtp(String email) {
        Optional<User> userOptional = userRepository.findByEmail(email);
        if (userOptional.isEmpty()) {
            return ResponseEntity.ok("Status 3000"); // Email not found

        }
        User user = userOptional.get();

        // Check user status
        if (!user.isActive()) {
            // User is disabled
            return ResponseEntity.ok("6000");
        }
        // Check level status
        if (user.getLevel() == null || !user.getLevel().isActive()) {
            // inactive level
            return ResponseEntity.ok("6000");
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
            return ResponseEntity.ok("Status 1000"); // OTP sent successfully

        } catch (MessagingException e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 2000"); // Email sending failed
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 9999"); // Unknown error
        }
    }

    // verify forgot password otp and reset the password
    public ResponseEntity<String> verifyResetCodeAndUpdatePassword(String email, String verificationCode,
            String newPassword) {
        String cacheKey = email + "reset";
        String storedCode = otpCache.get(cacheKey);

        try {
            if (storedCode == null || !storedCode.equals(verificationCode)) {
                return ResponseEntity.ok("Status 3000"); // Invalid OTP
            }

            Optional<User> userOptional = userRepository.findByEmail(email);
            if (userOptional.isEmpty()) {
                return ResponseEntity.ok("Status 3000"); // Email not found
            }

            if (newPassword == null || newPassword.isBlank()) {
                return ResponseEntity.ok("Status 4000"); // Password not provided
            }

            User user = userOptional.get();
            user.setPassword(encoder.encode(newPassword));
            userRepository.save(user);
            otpCache.remove(cacheKey);

            return ResponseEntity.ok("Status 1000"); // Password changed successfully

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 9999"); // Unknown error
        }
    }

    // Destroy the session
    public ResponseEntity<String> Logout() {
        userSession.removeAttribute("loggedInUser");
        userSession.invalidate();
        return ResponseEntity.ok("Status 1000");
    }

}
