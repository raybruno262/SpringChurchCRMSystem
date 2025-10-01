package com.SpringChurchCRMSystem.SpringChurchCRMSystem.service;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.BaptismInformation;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Member;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.RoleType;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.User;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.*;
import jakarta.servlet.http.HttpSession;

@Service
public class MemberService {

    @Autowired
    private MemberRepository memberRepository;
    @Autowired
    private LevelService levelService;
    @Autowired
    private DepartmentRepository departmentRepository;
    @Autowired
    private LevelRepository levelRepository;

    @Autowired
    private HttpSession userSession;

    public ResponseEntity<String> createMember(Member member, MultipartFile file) {
        try {
            User loggedInUser = (User) userSession.getAttribute("loggedInUser");

            // No user logged in
            if (loggedInUser == null) {
                return ResponseEntity.ok("Status 4000");
            }

            // Not a CellAdmin
            if (loggedInUser.getRole() != RoleType.CellAdmin) {
                return ResponseEntity.ok("Status 6000");
            }

            // Email already exists
            if (memberRepository.existsByEmail(member.getEmail())) {
                return ResponseEntity.ok("Status 5000");
            }

            // Department must exist
            if (member.getDepartment() == null || member.getDepartment().getDepartmentId() == null ||
                    !departmentRepository.existsByDepartmentId(member.getDepartment().getDepartmentId())) {
                return ResponseEntity.ok("Status 3000"); // Invalid department
            }

            // Baptism information
            BaptismInformation baptismInformation = member.getBaptismInformation();
            if (baptismInformation != null) {
                if (baptismInformation.isBaptized()) {
                    if (baptismInformation.isSameReligion()) {
                        // Must have a valid BaptismCell
                        if (baptismInformation.getBaptismCell() == null ||
                                baptismInformation.getBaptismCell().getLevelId() == null ||
                                !levelRepository.existsById(baptismInformation.getBaptismCell().getLevelId())) {
                            return ResponseEntity.ok("Status 3000"); // Invalid Baptism cell
                        }
                        baptismInformation.setOtherChurchName(null);
                        baptismInformation.setOtherChurchAddress(null);
                    } else {
                        // Name and Address of other Church
                        if (baptismInformation.getOtherChurchName() == null ||
                                baptismInformation.getOtherChurchAddress() == null) {
                            return ResponseEntity.ok("Status 3000"); // Missing other Church info
                        }

                        baptismInformation.setBaptismCell(null);
                    }
                } else {
                    // When not Baptized
                    if (baptismInformation != null && !baptismInformation.isBaptized()) {
                        baptismInformation.setSameReligion(false);
                        baptismInformation.setBaptismCell(null);
                        baptismInformation.setOtherChurchName(null);
                        baptismInformation.setOtherChurchName(null);
                    }
                }
            }
            // Handle profile picture
            if (file != null && !file.isEmpty()) {
                member.setProfilePic(file.getBytes());
            }

            // Member creation logic
            member.setMembershipDate(LocalDate.now());
            member.setLevel(loggedInUser.getLevel());
            member.setStatus("Active");

            memberRepository.save(member);

            // Success
            return ResponseEntity.ok("Status 1000");

        } catch (Exception e) {
            e.printStackTrace();

            // Internal server error
            return ResponseEntity.ok("Status 2000");
        }
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

    // Update all members
    public ResponseEntity<String> updateMember(String memberId, Member updatedData, MultipartFile file) {
        try {
            User loggedInUser = (User) userSession.getAttribute("loggedInUser");

            // No user logged in
            if (loggedInUser == null) {
                return ResponseEntity.ok("Status 4000");
            }

            // Not a CellAdmin
            if (loggedInUser.getRole() != RoleType.CellAdmin) {
                return ResponseEntity.ok("Status 6000");
            }

            Optional<Member> memberOpt = memberRepository.findById(memberId);
            if (memberOpt.isEmpty()) {
                return ResponseEntity.ok("Status 3000"); // Member not found
            }

            Member member = memberOpt.get();

            // Update fields if provided
            if (updatedData.getNames() != null)
                member.setNames(updatedData.getNames());
            if (updatedData.getDateOfBirth() != null)
                member.setDateOfBirth(updatedData.getDateOfBirth());
            if (updatedData.getPhone() != null)
                member.setPhone(updatedData.getPhone());
            if (updatedData.getGender() != null)
                member.setGender(updatedData.getGender());
            if (updatedData.getMaritalStatus() != null)
                member.setMaritalStatus(updatedData.getMaritalStatus());
            if (updatedData.getEmail() != null && !updatedData.getEmail().equals(member.getEmail())) {
                if (memberRepository.existsByEmail(updatedData.getEmail())) {
                    return ResponseEntity.ok("Status 5000"); // Email already exists
                }
                member.setEmail(updatedData.getEmail());
            }
            if (updatedData.getStatus() != null)
                member.setStatus(updatedData.getStatus());
            if (updatedData.getAddress() != null)
                member.setAddress(updatedData.getAddress());
            if (updatedData.getMembershipDate() != null)
                member.setMembershipDate(updatedData.getMembershipDate());
            if (updatedData.getLevel() != null)
                member.setLevel(updatedData.getLevel());

            // Validate and update department
            if (updatedData.getDepartment() != null &&
                    updatedData.getDepartment().getDepartmentId() != null &&
                    departmentRepository.existsByDepartmentId(updatedData.getDepartment().getDepartmentId())) {
                member.setDepartment(updatedData.getDepartment());
            }

            // Update profile picture if provided
            if (file != null && !file.isEmpty()) {
                member.setProfilePic(file.getBytes());
            }

            // Baptism information
            mergeBaptismInfo(member, updatedData.getBaptismInformation());

            memberRepository.save(member);
            return ResponseEntity.ok("Status 1000"); // Success

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 2000"); // Internal server error
        }
    }

    // mergeBaptismInfo for update
    private void mergeBaptismInfo(Member member, BaptismInformation incoming) {
        if (incoming == null)
            return;

        BaptismInformation existing = member.getBaptismInformation();
        if (existing == null) {
            existing = new BaptismInformation();
        }

        // Update baptized flag
        existing.setBaptized(incoming.isBaptized());

        // Update sameReligion flag
        existing.setSameReligion(incoming.isSameReligion());

        if (incoming.isBaptized()) {
            if (incoming.isSameReligion()) {
                // Validate and update baptismCell
                if (incoming.getBaptismCell() != null &&
                        incoming.getBaptismCell().getLevelId() != null &&
                        levelRepository.existsById(incoming.getBaptismCell().getLevelId())) {
                    existing.setBaptismCell(incoming.getBaptismCell());
                }
                // Clear otherChurch fields
                existing.setOtherChurchName(null);
                existing.setOtherChurchAddress(null);
            } else {
                // Update otherChurch fields if provided
                if (incoming.getOtherChurchName() != null) {
                    existing.setOtherChurchName(incoming.getOtherChurchName());
                }
                if (incoming.getOtherChurchAddress() != null) {
                    existing.setOtherChurchAddress(incoming.getOtherChurchAddress());
                }
                // Clear baptismCell
                existing.setBaptismCell(null);
            }
        } else {
            // Not baptized — clear all conditional fields
            existing.setSameReligion(false);
            existing.setBaptismCell(null);
            existing.setOtherChurchName(null);
            existing.setOtherChurchAddress(null);
        }

        member.setBaptismInformation(existing);
    }

    // Get scoped members
    public Page<Member> getScopedPaginatedMembers(int page, int size) {
        User loggedInUser = (User) userSession.getAttribute("loggedInUser");
        if (loggedInUser == null) {
            return Page.empty();
        }

        // SuperAdmin sees every member
        if (loggedInUser.getRole() == RoleType.SuperAdmin) {
            return memberRepository.findAll(PageRequest.of(page, size));
        }

        // Get all Active cells under the user's level and include their own level
        List<Level> scopedLevels = levelService.getAllActiveCellsUnder(loggedInUser.getLevel());
        scopedLevels.add(loggedInUser.getLevel());

        // Extract level IDs for reliable querying
        List<String> levelIds = scopedLevels.stream()
                .map(Level::getLevelId)
                .toList();

        // Paginated query by level ID
        PageRequest pageable = PageRequest.of(page, size);
        return memberRepository.findByLevel_LevelIdIn(levelIds, pageable);
    }

    // Get scoped Birthdays
    public Page<Member> getScopedMembersWithBirthdaysThisMonth(int page, int size) {
        User loggedInUser = (User) userSession.getAttribute("loggedInUser");
        if (loggedInUser == null) {
            return Page.empty();
        }
        // Get current Month
        int currentMonth = LocalDate.now().getMonthValue();

        // paginated request
        PageRequest pageable = PageRequest.of(page, size);

        // SuperAdmin sees all birthdays
        if (loggedInUser.getRole() == RoleType.SuperAdmin) {
            Page<Member> allMembers = memberRepository.findAll(pageable);
            List<Member> filtered = allMembers.getContent().stream().filter(member -> member.getDateOfBirth() != null &&
                    member.getDateOfBirth().getMonthValue() == currentMonth).toList();
            return new PageImpl<>(filtered, pageable, filtered.size());

        }
        // scoped levels

        // Scoped Active levels for CellAdmin
        List<Level> scopedLevels = levelService.getAllActiveCellsUnder(loggedInUser.getLevel());
        scopedLevels.add(loggedInUser.getLevel());
        List<String> levelIds = scopedLevels.stream()
                .map(Level::getLevelId)
                .toList();

        // Scoped Birthday Members
        // Scoped members
        Page<Member> scopedMembers = memberRepository.findByLevel_LevelIdIn(levelIds, pageable);
        List<Member> filtered = scopedMembers.getContent().stream()
                .filter(member -> member.getDateOfBirth() != null &&
                        member.getDateOfBirth().getMonthValue() == currentMonth)
                .toList();

        return new PageImpl<>(filtered, pageable, filtered.size());

    }

}
