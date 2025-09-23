package com.SpringChurchCRMSystem.SpringChurchCRMSystem.service;

import java.time.LocalDate;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Member;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.User;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.MemberRepository;

import jakarta.servlet.http.HttpSession;

@Service
public class MemberService {
    @Autowired
    private MemberRepository memberRepository;

    @Autowired
    private HttpSession userSession;

    // create member
    public String createMember(Member member) {
        User loggedInUser = (User) userSession.getAttribute("loggedInUser");
        if (loggedInUser == null) {
            return "No user is currently logged in";
        }
        if (memberRepository.existsByEmail(member.getEmail())) {
            return "Email already exists";
        }
        member.setMembershipDate(LocalDate.now());
        member.setLevel(loggedInUser.getLevel());
        memberRepository.save(member);
        return "Member saved succefully";
    }

    // Find all members
    public List<Member> getAllMembers() {
        return memberRepository.findAll();
    }

    // Find all paginated members
    public Page<Member> getPaginatedMembers(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size);
        return memberRepository.findAll(pageable);

    }

    // Delete all members
    public String deleteMember(String memberId) {
        if (!memberRepository.existsById(memberId)) {
            return "Id does not exists";
        }
        memberRepository.deleteById(memberId);
        return "member deleted successfully";
    }

    // Update all members
    public String updateMember(String memberId, Member member) {
        if (!memberRepository.existsById(memberId)) {
            return "Id not found";
        }
        member.setMemberId(memberId);
        memberRepository.save(member);
        return "Member Updated Successfully";
    }

}
