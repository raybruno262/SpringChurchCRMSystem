package com.SpringChurchCRMSystem.SpringChurchCRMSystem.service;

import java.time.LocalDate;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.Dto.MemberStatsDTO;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.BaptismInformation;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.LevelType;
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
    private UserRepository userRepository;

    public ResponseEntity<String> createMember(Member member, MultipartFile file, String userId) {
        try {
            User loggedInUser = userRepository.findByUserId(userId);

            // No user logged in
            if (loggedInUser == null) {
                return ResponseEntity.ok("Status 4000");
            }

            // Not a CellAdmin or admin
            if (loggedInUser.getRole() != RoleType.CellAdmin && loggedInUser.getRole() != RoleType.SuperAdmin) {
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

    public Member getMemberById(String memberId) {
        return memberRepository.findById(memberId).orElse(null);
    }

    // Find all members
    public List<Member> getAllMembers() {
        return memberRepository.findAll(Sort.by(Sort.Direction.DESC, "memberId"));
    }

    // Find all paginated members
    public Page<Member> getPaginatedMembers(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "userId"));
        return memberRepository.findAll(pageable);

    }

    // Update all members
    public ResponseEntity<String> updateMember(String memberId, Member updatedData, MultipartFile file, String userId) {
        try {
            User loggedInUser = userRepository.findByUserId(userId);
            // No user logged in
            if (loggedInUser == null) {
                return ResponseEntity.ok("Status 4000");
            }

            // Not a CellAdmin
            if (loggedInUser.getRole() != RoleType.CellAdmin && loggedInUser.getRole() != RoleType.SuperAdmin) {
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
    public Page<Member> getScopedPaginatedMembers(int page, int size, String userId) {
        User loggedInUser = userRepository.findByUserId(userId);
        if (loggedInUser == null) {
            return Page.empty();
        }

        PageRequest pageable = PageRequest.of(page, size);

        if (loggedInUser.getRole() == RoleType.SuperAdmin) {
            return memberRepository.findAll(pageable);
        }

        List<Level> scopedCells = levelService.getAllCellsUnder(loggedInUser.getLevel());

        if (loggedInUser.getLevel().getLevelType() == LevelType.CELL &&
                !scopedCells.contains(loggedInUser.getLevel())) {
            scopedCells.add(loggedInUser.getLevel());
        }

        return memberRepository.findByLevelIn(scopedCells, pageable);
    }

    // Get scoped Birthdays
public Page<Member> getScopedMembersWithBirthdaysThisMonth(int page, int size, String userId) {
    User loggedInUser = userRepository.findByUserId(userId);
    if (loggedInUser == null) {
        return Page.empty();
    }

    int currentMonth = LocalDate.now().getMonthValue();
    PageRequest pageable = PageRequest.of(page, size);

    List<Member> scopedMembers;

    if (loggedInUser.getRole() == RoleType.SuperAdmin) {
        scopedMembers = memberRepository.findAll();
    } else {
        List<Level> scopedCells = levelService.getAllCellsUnder(loggedInUser.getLevel());

        if (loggedInUser.getLevel().getLevelType() == LevelType.CELL &&
                !scopedCells.contains(loggedInUser.getLevel())) {
            scopedCells.add(loggedInUser.getLevel());
        }

        scopedMembers = memberRepository.findByLevelIn(scopedCells);
    }

    // Filter by birthday month BEFORE pagination
    List<Member> birthdayMembers = scopedMembers.stream()
        .filter(member -> member.getDateOfBirth() != null &&
                member.getDateOfBirth().getMonthValue() == currentMonth)
        .sorted(Comparator.comparing(Member::getDateOfBirth)) // Optional: sort by birthday
        .toList();

    //  Apply manual pagination
    int start = Math.min(page * size, birthdayMembers.size());
    int end = Math.min(start + size, birthdayMembers.size());
    List<Member> paginated = birthdayMembers.subList(start, end);

    return new PageImpl<>(paginated, pageable, birthdayMembers.size());
}

    // unpaginated birthdays
    public List<Member> getUnpaginatedScopedMembersWithBirthdaysThisMonth(String userId) {
        User loggedInUser = userRepository.findByUserId(userId);
        if (loggedInUser == null) {
            return Collections.emptyList();
        }

        int currentMonth = LocalDate.now().getMonthValue();

        if (loggedInUser.getRole() == RoleType.SuperAdmin) {
            List<Member> allMembers = memberRepository.findAll();
            return allMembers.stream()
                    .filter(member -> member.getDateOfBirth() != null &&
                            member.getDateOfBirth().getMonthValue() == currentMonth)
                    .toList();
        }

        List<Level> scopedCells = levelService.getAllCellsUnder(loggedInUser.getLevel());

        if (loggedInUser.getLevel().getLevelType() == LevelType.CELL &&
                !scopedCells.contains(loggedInUser.getLevel())) {
            scopedCells.add(loggedInUser.getLevel());
        }

        List<Member> scopedMembers = memberRepository.findByLevelIn(scopedCells);
        return scopedMembers.stream()
                .filter(member -> member.getDateOfBirth() != null &&
                        member.getDateOfBirth().getMonthValue() == currentMonth)
                .toList();
    }

    // member stats data
    public MemberStatsDTO getScopedMemberStats(String userId) {
        User loggedInUser = userRepository.findByUserId(userId);
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
            // ✅ Use scoped CELL levels (DBRef Level objects)
            List<Level> scopedCells = levelService.getAllCellsUnder(loggedInUser.getLevel());

            if (loggedInUser.getLevel().getLevelType() == LevelType.CELL &&
                    !scopedCells.contains(loggedInUser.getLevel())) {
                scopedCells.add(loggedInUser.getLevel());
            }

            // ✅ Count using DBRef Level objects
            total = memberRepository.countByLevelIn(scopedCells);
            active = memberRepository.countByStatusAndLevelIn("Active", scopedCells);
            inactive = memberRepository.countByStatusAndLevelIn("Inactive", scopedCells);
            transferred = memberRepository.countByStatusAndLevelIn("Transferred", scopedCells);
        }

        return new MemberStatsDTO(total, active, inactive, transferred);
    }

    // Total birthdays within a month
    public long getScopedBirthdayCountThisMonth(String userId) {
        User loggedInUser = userRepository.findByUserId(userId);
        if (loggedInUser == null) {
            return 0;
        }

        int currentMonth = LocalDate.now().getMonthValue();

        if (loggedInUser.getRole() == RoleType.SuperAdmin) {
            List<Member> allMembers = memberRepository.findAll();
            return allMembers.stream()
                    .filter(member -> member.getDateOfBirth() != null &&
                            member.getDateOfBirth().getMonthValue() == currentMonth)
                    .count();
        }

        List<Level> scopedCells = levelService.getAllCellsUnder(loggedInUser.getLevel());

        if (loggedInUser.getLevel().getLevelType() == LevelType.CELL &&
                !scopedCells.contains(loggedInUser.getLevel())) {
            scopedCells.add(loggedInUser.getLevel());
        }

        List<Member> scopedMembers = memberRepository.findByLevelIn(scopedCells);
        return scopedMembers.stream()
                .filter(member -> member.getDateOfBirth() != null &&
                        member.getDateOfBirth().getMonthValue() == currentMonth)
                .count();
    }

}
