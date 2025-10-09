package com.SpringChurchCRMSystem.SpringChurchCRMSystem.controller;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.User;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.service.UserService;

import jakarta.servlet.http.HttpSession;
import lombok.RequiredArgsConstructor;

@RestController
@RequiredArgsConstructor
@RequestMapping("api/users")
public class UserController {

    private final UserService userService;
    private final HttpSession userSession;

    // create a user ( form-data)
    @PostMapping("/createrUser")
    public ResponseEntity<String> createUser(
            @RequestPart("user") User user,
            @RequestPart(value = "file", required = false) MultipartFile file) {

        return userService.createUser(user, file);

    }

    // update a user
    @PutMapping("/updateUser/{userId}")
    public ResponseEntity<String> updateUserWithPic(
            @PathVariable String userId,
            @RequestPart("user") User updatedData,
            @RequestPart(value = "file", required = false) MultipartFile file) {

        return userService.updateUser(userId, updatedData, file);

    }

    // get all users regardless of the status
    @GetMapping("/allUsers")
    public List<User> getAllUsers() {

        return userService.getAllUsers();

    }

    // get all paginated users
    @GetMapping("/getPaginatedUsers")
    public Page<User> getPaginatedUsers(@RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {

        return userService.getPaginatedUsers(page, size);

    }

    @PostMapping("/sendPasswordResetOtp")
    public ResponseEntity<String> sendPasswordResetOtp(@RequestParam String email) {

        return userService.sendPasswordResetOtp(email);

    }

    @PostMapping("/resetPassword")
    public ResponseEntity<String> resetPassword(@RequestParam String email,
            @RequestParam String verificationCode, @RequestParam String newPassword) {

        return userService.verifyResetCodeAndUpdatePassword(email, verificationCode, newPassword);

    }

    @PostMapping("/sendLoginOtp")
    public ResponseEntity<String> sendLogintOtp(@RequestParam String email, @RequestParam String Password) {

        return userService.sendLoginOtp(email, Password);

    }

    @PostMapping("/login")
    public ResponseEntity<?> login(@RequestParam String email,
            @RequestParam String verifyCode, @RequestParam String Password) {

        return userService.verifyLoginCodeAndLogin(email, verifyCode, Password);

    }

    // Destroy the session
    @PostMapping("/destroySession")
    public ResponseEntity<Void> logout() {
        userSession.removeAttribute("loggedInUser");
        userSession.invalidate();
        return ResponseEntity.ok().build();
    }

}
