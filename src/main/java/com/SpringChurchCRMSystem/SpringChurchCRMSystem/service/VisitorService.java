package com.SpringChurchCRMSystem.SpringChurchCRMSystem.service;

import java.time.LocalDate;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;

import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.Dto.VisitorStatsDTO;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.FollowUp;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.LevelType;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.RoleType;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.User;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Visitor;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.*;

@Service
public class VisitorService {

    @Autowired
    private VisitorRepository visitorRepository;
    @Autowired
    private LevelService levelService;
    @Autowired
    private LevelRepository levelRepository;

    @Autowired
    private UserRepository userRepository;

    // create visitor
    public ResponseEntity<String> createVisitor(Visitor visitor, String userId) {
        try {
            User loggedInUser = userRepository.findByUserId(userId);

            // No user logged in
            if (loggedInUser == null) {
                return ResponseEntity.ok("Status 4000");
            }

            // Not a CellAdmin or SuperAdmin
            if (loggedInUser.getRole() != RoleType.CellAdmin && loggedInUser.getRole() != RoleType.SuperAdmin) {
                return ResponseEntity.ok("Status 6000");
            }

            // Level must exist
            if (visitor.getLevel() == null || visitor.getLevel().getLevelId() == null ||
                    !levelRepository.existsById(visitor.getLevel().getLevelId())) {
                return ResponseEntity.ok("Status 3000"); // Invalid level
            }

            // Visit date fallback
            if (visitor.getVisitDate() == null) {
                visitor.setVisitDate(LocalDate.now());
            }

            // Default status
            if (visitor.getStatus() == null || visitor.getStatus().isBlank()) {
                visitor.setStatus("New");
            }

            // Save visitor
            visitorRepository.save(visitor);

            return ResponseEntity.ok("Status 1000"); // Success

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 2000"); // Internal server error
        }
    }

    public ResponseEntity<String> updateVisitor(String visitorId, Visitor updatedData, String userId) {
        try {
            User loggedInUser = userRepository.findByUserId(userId);

            if (loggedInUser == null) {
                return ResponseEntity.ok("Status 4000");
            }

            if (loggedInUser.getRole() != RoleType.CellAdmin && loggedInUser.getRole() != RoleType.SuperAdmin) {
                return ResponseEntity.ok("Status 6000");
            }

            Optional<Visitor> visitorOpt = visitorRepository.findById(visitorId);
            if (visitorOpt.isEmpty()) {
                return ResponseEntity.ok("Status 3000"); // Visitor not found
            }

            Visitor visitor = visitorOpt.get();

            if (updatedData.getNames() != null)
                visitor.setNames(updatedData.getNames());
            if (updatedData.getPhone() != null)
                visitor.setPhone(updatedData.getPhone());
            if (updatedData.getGender() != null)
                visitor.setGender(updatedData.getGender());
            if (updatedData.getEmail() != null && !updatedData.getEmail().equals(visitor.getEmail()))
                visitor.setEmail(updatedData.getEmail());
            if (updatedData.getAddress() != null)
                visitor.setAddress(updatedData.getAddress());
            if (updatedData.getVisitDate() != null)
                visitor.setVisitDate(updatedData.getVisitDate());
            if (updatedData.getStatus() != null)
                visitor.setStatus(updatedData.getStatus());

            if (updatedData.getLevel() != null &&
                    updatedData.getLevel().getLevelId() != null &&
                    levelRepository.existsById(updatedData.getLevel().getLevelId())) {
                visitor.setLevel(updatedData.getLevel());
            }

            visitorRepository.save(visitor);
            return ResponseEntity.ok("Status 1000"); // Success

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 2000"); // Internal server error
        }
    }

    // get scoped paginated visitors
    public Page<Visitor> getScopedPaginatedVisitors(int page, int size, String userId) {
        User loggedInUser = userRepository.findByUserId(userId);
        if (loggedInUser == null) {
            return Page.empty();
        }

        PageRequest pageable = PageRequest.of(page, size);

        if (loggedInUser.getRole() == RoleType.SuperAdmin) {
            return visitorRepository.findAll(pageable);
        }

        List<Level> scopedCells = levelService.getAllCellsUnder(loggedInUser.getLevel());

        if (loggedInUser.getLevel().getLevelType() == LevelType.CELL &&
                !scopedCells.contains(loggedInUser.getLevel())) {
            scopedCells.add(loggedInUser.getLevel());
        }

        return visitorRepository.findByLevelIn(scopedCells, pageable);
    }

    public ResponseEntity<String> addFollowUp(String visitorId, FollowUp followUp) {
        Optional<Visitor> optionalVisitor = visitorRepository.findById(visitorId);
        if (optionalVisitor.isEmpty()) {
            return ResponseEntity.ok("Status 3000"); // Visitor not found
        }

        Visitor visitor = optionalVisitor.get();

        // Add the new follow-up
        visitor.getFollowUps().add(followUp);

        if (visitor.getStatus() == null || !visitor.getStatus().equalsIgnoreCase("Follow-up")) {
            visitor.setStatus("Follow-up");
        }

        visitorRepository.save(visitor);
        return ResponseEntity.ok("Status 1000"); // Success
    }

    // get visitor by id

    public Visitor getVisitorById(String visitorId) {
        return visitorRepository.findById(visitorId).orElse(null);
    }

    // Find all Visitors
    public List<Visitor> getAllVisitors() {
        return visitorRepository.findAll(Sort.by(Sort.Direction.DESC, "visitorId"));
    }

    // Find all paginated visitors
    public Page<Visitor> getPaginatedVisitors(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "visitorId"));
        return visitorRepository.findAll(pageable);

    }
    // visitor stats

    public VisitorStatsDTO getScopedVisitorStats(String userId) {
        User loggedInUser = userRepository.findByUserId(userId);
        if (loggedInUser == null) {
            return new VisitorStatsDTO(0, 0, 0, 0,0);
        }

        long total;
        long newVisitors;
        long followedUp;
        long converted;
        long dropped;

        if (loggedInUser.getRole() == RoleType.SuperAdmin) {
            total = visitorRepository.count();
            newVisitors = visitorRepository.countByStatus("New");
            followedUp = visitorRepository.countByStatus("Follow-up");
            converted = visitorRepository.countByStatus("Converted");
            dropped = visitorRepository.countByStatus("Dropped");
        } else {
            List<Level> scopedCells = levelService.getAllCellsUnder(loggedInUser.getLevel());

            if (loggedInUser.getLevel().getLevelType() == LevelType.CELL &&
                    !scopedCells.contains(loggedInUser.getLevel())) {
                scopedCells.add(loggedInUser.getLevel());
            }

            total = visitorRepository.countByLevelIn(scopedCells);
            newVisitors = visitorRepository.countByStatusAndLevelIn("New", scopedCells);
            followedUp = visitorRepository.countByStatusAndLevelIn("Follow-up", scopedCells);
            converted = visitorRepository.countByStatusAndLevelIn("Converted", scopedCells);
            dropped = visitorRepository.countByStatusAndLevelIn("Dropped", scopedCells);
        }

        return new VisitorStatsDTO(total, newVisitors, followedUp, converted, dropped);
    }

}
