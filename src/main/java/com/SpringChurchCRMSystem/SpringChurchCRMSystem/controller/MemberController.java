package com.SpringChurchCRMSystem.SpringChurchCRMSystem.controller;

import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.Dto.MemberStatsDTO;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Member;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.service.MemberService;

@RestController
@RequestMapping("/api/members")
public class MemberController {

    @Autowired
    private MemberService memberService;

    // create a user ( form-data)
    @PostMapping("/createMember")
    public ResponseEntity<String> createUser(
            @RequestPart("member") Member member,
            @RequestPart(value = "file", required = false) MultipartFile file) {

        return memberService.createMember(member, file);

    }

    @GetMapping("/allMembers")
    public List<Member> getAllMembers() {

        return memberService.getAllMembers();

    }

    @PutMapping("/updateMember/{memberId}")
    public ResponseEntity<String> updateMember(
            @PathVariable String memberId,
            @RequestPart("member") Member updatedData,
            @RequestPart(value = "file", required = false) MultipartFile file) {

        return memberService.updateMember(memberId, updatedData, file);

    }

    @GetMapping("/paginatedMembers")
    public Page<Member> getPaginatedMembers(@RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {

        return memberService.getPaginatedMembers(page, size);

    }

    // Scoped paginated members based on hierarchy
    @GetMapping("/scopedPaginatedMembers")
    public Page<Member> getScopedPaginatedMembers(
            @RequestParam String userId,
            @RequestParam String levelId,
            @RequestParam int page,
            @RequestParam int size) {
        return memberService.getScopedPaginatedMembers(userId, levelId, page, size);
    }

    // Scoped birthday members for current month
    @GetMapping("/scopedBirthdayMembers")
    public Page<Member> getScopedBirthdayMembers(@RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {

        return memberService.getScopedMembersWithBirthdaysThisMonth(page, size);

    }

    // member stats data
    @GetMapping("/stats")
    public MemberStatsDTO getScopedMemberStats(@RequestParam String userId) {
        return memberService.getScopedMemberStats(userId);
    }

}