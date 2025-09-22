package com.SpringChurchCRMSystem.SpringChurchCRMSystem.controller;

import org.springframework.web.bind.annotation.*;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Member;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.service.MemberService;

@RestController
@RequestMapping("/api/members")
public class MemberController {

    @Autowired
    private MemberService memberService;

    @PostMapping("/createMember")
    public String createMember(@RequestBody Member member) {
        return memberService.createMember(member);
    }

    @GetMapping("/allMembers")
    public List<Member> getAllMembers() {
        return memberService.getAllMembers();
    }

    @PutMapping("/updateMember/{memberId}")
    public String updateMember(@PathVariable String memberId, @RequestBody Member member) {
        return memberService.updateMember(memberId, member);
    }

    @DeleteMapping("/deleteMember/{memberId}")
    public String deleteMember(@PathVariable String memberId) {
        return memberService.deleteMember(memberId);
    }

    @GetMapping("/paginatedMembers")
    public Page<Member> getPaginatedMembers(@RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {
        return memberService.getPaginatedMembers(page, size);
    }

}