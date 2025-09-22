package com.SpringChurchCRMSystem.SpringChurchCRMSystem.service;

import java.util.List;
import java.util.Optional;
import java.util.Random;
import java.util.concurrent.ConcurrentHashMap;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.User;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.UserRepository;
import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class UserService {
    @Autowired
    private UserRepository userRepository;
    private final ConcurrentHashMap<String, String> otpCache = new ConcurrentHashMap<>();
    private final JavaMailSender emailDispatcher;

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

    // Generating random numbers
    private String generateRandomCode() {
        return String.valueOf(100000 + new Random().nextInt(900000));
    }

    // Send Reset Password OTP
    public String sendPasswordResetOtp(String email) {
        Optional<User> userOptional = userRepository.findByEmail(email);
        if (userOptional.isEmpty()) {
            return "Email not found";
        } else {
            String verificationCode = generateRandomCode();
            otpCache.put(email + "reset", verificationCode);
            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(email);
            message.setSubject("Your Password Reset Code");
            message.setText("Your password reset verification code is: " + verificationCode);
            emailDispatcher.send(message);

            return "Otp sent";
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

            return "Password Changed Successfully";
        } else {
            return "Password not saved";
        }
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
