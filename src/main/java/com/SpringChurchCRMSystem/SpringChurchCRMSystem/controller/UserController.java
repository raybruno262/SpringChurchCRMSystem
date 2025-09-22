package com.SpringChurchCRMSystem.SpringChurchCRMSystem.controller;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.User;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.service.UserService;

import lombok.RequiredArgsConstructor;

@RestController
@RequiredArgsConstructor
@RequestMapping("api/users")
public class UserController {

    private final UserService userService;

    @PostMapping("/createuser")
    public String createUser(@RequestBody User user) {
        return userService.createUser(user);
    }

    @PostMapping("/sendPasswordResetOtp")
    public String sendPasswordResetOtp(@RequestParam String email) {
        return userService.sendPasswordResetOtp(email);
    }

    @PostMapping("/resetPassword")
    public String resetPassword(@RequestParam String email,
            @RequestParam String verificationCode, @RequestParam String newPassword) {
        return userService.verifyResetCodeAndUpdatePassword(email, verificationCode, newPassword);
    }

    @PutMapping("/updateUser/{userId}")
    public String updateUser(@PathVariable String userId, @RequestBody User user) {
        return userService.updateUser(userId, user);
    }

    @DeleteMapping("/deleteUser/{userId}")
    public String deleteUser(@PathVariable String userId) {
        return userService.deleteUser(userId);
    }

    @GetMapping("/allUsers")
    public List<User> getAllUsers() {
        return userService.getAllUsers();
    }

    @GetMapping("/paginatedUsers")
    public Page<User> getPaginatedUsers(@RequestParam )

}
