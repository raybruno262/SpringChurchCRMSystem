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

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.Dto.MemberStatsDTO;
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
    @Autowired
    private UserRepository userRepository;

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

            // Department must exist
            if (member.getDepartment() == null || member.getDepartment().getDepartmentId() == null ||
                    !departmentRepository.existsByDepartmentId(member.getDepartment().getDepartmentId())) {
                return ResponseEntity.ok("Status 3000"); // Invalid department
            }

            // Baptism information
            BaptismInformation baptismInformation = member.getBaptismInformation();
            if (baptismInformation != null) {

                if (Boolean.TRUE.equals(baptismInformation.getBaptized())) {

                    if (Boolean.TRUE.equals(baptismInformation.getSameReligion())) {
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
                    if (baptismInformation != null && Boolean.FALSE.equals(baptismInformation.getBaptized())) {
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

        if (Boolean.TRUE.equals(incoming.getBaptized())) {
            existing.setBaptized(incoming.getBaptized());

            if (Boolean.TRUE.equals(incoming.getSameReligion())) {
                existing.setSameReligion(incoming.getSameReligion());
                // Must have a valid BaptismCell
                if (incoming.getBaptismCell() != null &&
                        incoming.getBaptismCell().getLevelId() != null &&
                        levelRepository.existsById(incoming.getBaptismCell().getLevelId())) {
                    existing.setBaptismCell(incoming.getBaptismCell());
                }
                existing.setOtherChurchName(null);
                existing.setOtherChurchAddress(null);

            } else if (Boolean.FALSE.equals(incoming.getSameReligion())) {
                existing.setSameReligion(incoming.getSameReligion());

                // Name and Address of other Church
                if (incoming.getOtherChurchName() != null ||
                        incoming.getOtherChurchAddress() != null) {
                    existing.setOtherChurchName(incoming.getOtherChurchName());
                    existing.setOtherChurchAddress(incoming.getOtherChurchAddress());
                }

                existing.setBaptismCell(null);
            }
        } else if (incoming != null && Boolean.FALSE.equals(incoming.getBaptized())) {
            // When not Baptized

            existing.setSameReligion(false);
            existing.setBaptismCell(null);
            existing.setOtherChurchName(null);
            existing.setOtherChurchName(null);
        }

        member.setBaptismInformation(existing);
    }

    // Get scoped members
    public Page<Member> getScopedPaginatedMembers(int page, int size) {
        User loggedInUser = (User) userSession.getAttribute("loggedInUser");
        if (loggedInUser == null) {
            return Page.empty();
        }
        System.out.println("Logged-in user: " + loggedInUser.getUserId());
        System.out.println("User level: " + loggedInUser.getLevel());
        System.out.println("User role: " + loggedInUser.getRole());

        PageRequest pageable = PageRequest.of(page, size);

        if (loggedInUser.getRole() == RoleType.SuperAdmin) {
            return memberRepository.findAll(pageable);
        }

        List<Level> scopedLevels = levelService.getAllLevelsUnder(loggedInUser.getLevel());
        scopedLevels.add(loggedInUser.getLevel());

        List<String> levelIds = scopedLevels.stream()
                .map(Level::getLevelId)
                .toList();
        System.out.println("Scoped level IDs: " + scopedLevels.stream().map(Level::getLevelId).toList());

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
            Page<Member> allMembers = memberRepository.findByStatus("Active", PageRequest.of(page, size));

            List<Member> filtered = allMembers.getContent().stream().filter(member -> member.getDateOfBirth() != null &&
                    member.getDateOfBirth().getMonthValue() == currentMonth).toList();
            return new PageImpl<>(filtered, pageable, filtered.size());

        }

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

    // member stats data
    public MemberStatsDTO getScopedMemberStats(String userId) {
        User loggedInUser = userRepository.findById(userId).orElse(null);
        if (loggedInUser == null) {
            return new MemberStatsDTO(0, 0, 0, 0);
        }

        long total;
        long active;
        long inactive;
        long transferred;

        if (loggedInUser.getRole() == RoleType.SuperAdmin) {
            total = memberRepository.count();
            active = memberRepository.countByStatus("Active");
            inactive = memberRepository.countByStatus("Inactive");
            transferred = memberRepository.countByStatus("Transferred");
        } else {
            List<Level> scopedLevels = levelService.getAllActiveCellsUnder(loggedInUser.getLevel());
            scopedLevels.add(loggedInUser.getLevel());

            List<String> scopedLevelIds = scopedLevels.stream()
                    .map(Level::getLevelId)
                    .toList();

            total = memberRepository.countByLevel_LevelIdIn(scopedLevelIds);
            active = memberRepository.countByStatusAndLevel_LevelIdIn("Active", scopedLevelIds);
            inactive = memberRepository.countByStatusAndLevel_LevelIdIn("Inactive", scopedLevelIds);
            transferred = memberRepository.countByStatusAndLevel_LevelIdIn("Transferred", scopedLevelIds);
        }

        return new MemberStatsDTO(total, active, inactive, transferred);
    }

}
