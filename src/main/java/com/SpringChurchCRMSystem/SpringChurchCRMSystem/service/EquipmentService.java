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

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.Dto.EquipmentStatsDTO;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Equipment;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.EquipmentCategory;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.LevelType;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.RoleType;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.User;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.EquipmentCategoryRepository;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.EquipmentRepository;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.UserRepository;

@Service
public class EquipmentService {

    @Autowired
    private EquipmentRepository equipmentRepository;

    @Autowired
    private EquipmentCategoryRepository equipmentCategoryRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private LevelService levelService;

    public ResponseEntity<String> createEquipment(Equipment equipment, String userId) {
        try {
            User loggedInUser = userRepository.findByUserId(userId);

            // No user logged in
            if (loggedInUser == null) {
                return ResponseEntity.ok("Status 4000");
            }

            // Not authorized (only SuperAdmin and CellAdmin can create equipment)
            if (loggedInUser.getRole() != RoleType.CellAdmin && loggedInUser.getRole() != RoleType.SuperAdmin) {
                return ResponseEntity.ok("Status 6000");
            }

            // Validate required fields
            if (equipment.getName() == null || equipment.getName().trim().isEmpty()) {
                return ResponseEntity.ok("Status 3000"); // Name is required
            }

            // Validate equipment category
            if (equipment.getEquipmentCategory() == null ||
                    equipment.getEquipmentCategory().getEquipmentCategoryId() == null ||
                    !equipmentCategoryRepository
                            .existsById(equipment.getEquipmentCategory().getEquipmentCategoryId())) {
                return ResponseEntity.ok("Status 3000"); // Invalid equipment category
            }

            // Validate level (equipment must be associated with a level)
            if (equipment.getLevel() == null || equipment.getLevel().getLevelId() == null) {
                return ResponseEntity.ok("Status 3000"); // Level is required
            }

            // Validate purchase date (cannot be in the future)
            if (equipment.getPurchaseDate() != null && equipment.getPurchaseDate().isAfter(LocalDate.now())) {
                return ResponseEntity.ok("Status 3000"); // Invalid purchase date
            }

            // Validate purchase price
            if (equipment.getPurchasePrice() < 0) {
                return ResponseEntity.ok("Status 3000"); // Invalid purchase price
            }

            // Validate condition
            if (equipment.getCondition() == null || equipment.getCondition().trim().isEmpty()) {
                return ResponseEntity.ok("Status 3000"); // Condition is required
            }

            // Validate condition values
            String condition = equipment.getCondition();
            if (!condition.equals("Excellent") && !condition.equals("Good") &&
                    !condition.equals("Needs Repair") && !condition.equals("Out of Service")) {
                return ResponseEntity.ok("Status 3000"); // Invalid condition
            }

            // Set default purchase date if not provided
            if (equipment.getPurchaseDate() == null) {
                equipment.setPurchaseDate(LocalDate.now());
            }

            equipmentRepository.save(equipment);
            return ResponseEntity.ok("Status 1000"); // Success

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 2000"); // Internal server error
        }
    }

    public Equipment getEquipmentById(String equipmentId) {
        return equipmentRepository.findById(equipmentId).orElse(null);
    }

    // Find all equipment records
    public List<Equipment> getAllEquipment() {
        return equipmentRepository.findAll(Sort.by(Sort.Direction.DESC, "purchaseDate"));
    }

    // Find all paginated equipment records
    public Page<Equipment> getPaginatedEquipment(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "purchaseDate"));
        return equipmentRepository.findAll(pageable);
    }

    // Update equipment record
    public ResponseEntity<String> updateEquipment(String equipmentId, Equipment updatedData, String userId) {
        try {
            User loggedInUser = userRepository.findByUserId(userId);

            // No user logged in
            if (loggedInUser == null) {
                return ResponseEntity.ok("Status 4000");
            }

            // Not authorized
            if (loggedInUser.getRole() != RoleType.CellAdmin && loggedInUser.getRole() != RoleType.SuperAdmin) {
                return ResponseEntity.ok("Status 6000");
            }

            Optional<Equipment> equipmentOpt = equipmentRepository.findById(equipmentId);
            if (equipmentOpt.isEmpty()) {
                return ResponseEntity.ok("Status 3000"); // Equipment not found
            }

            Equipment equipment = equipmentOpt.get();

            // Update fields if provided
            if (updatedData.getName() != null) {
                equipment.setName(updatedData.getName());
            }

            if (updatedData.getEquipmentCategory() != null) {
                // Validate equipment category
                if (updatedData.getEquipmentCategory().getEquipmentCategoryId() != null &&
                        equipmentCategoryRepository
                                .existsById(updatedData.getEquipmentCategory().getEquipmentCategoryId())) {
                    equipment.setEquipmentCategory(updatedData.getEquipmentCategory());
                } else {
                    return ResponseEntity.ok("Status 3000"); // Invalid equipment category
                }
            }

            // Update level if provided
            if (updatedData.getLevel() != null) {
                equipment.setLevel(updatedData.getLevel());
            }

            if (updatedData.getPurchaseDate() != null) {
                // Validate purchase date
                if (updatedData.getPurchaseDate().isAfter(LocalDate.now())) {
                    return ResponseEntity.ok("Status 3000"); // Invalid purchase date
                }
                equipment.setPurchaseDate(updatedData.getPurchaseDate());
            }

            if (updatedData.getPurchasePrice() >= 0) {
                equipment.setPurchasePrice(updatedData.getPurchasePrice());
            }

            if (updatedData.getCondition() != null) {
                // Validate condition
                String condition = updatedData.getCondition();
                if (condition.equals("Excellent") || condition.equals("Good") ||
                        condition.equals("Needs Repair") || condition.equals("Out of Service")) {
                    equipment.setCondition(condition);
                } else {
                    return ResponseEntity.ok("Status 3000"); // Invalid condition
                }
            }

            if (updatedData.getLocation() != null) {
                equipment.setLocation(updatedData.getLocation());
            }

            if (updatedData.getDescription() != null) {
                equipment.setDescription(updatedData.getDescription());
            }

            equipmentRepository.save(equipment);
            return ResponseEntity.ok("Status 1000"); // Success

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 2000"); // Internal server error
        }
    }

    // Get scoped equipment records (based on user's level/role)
    public Page<Equipment> getScopedPaginatedEquipment(int page, int size, String userId) {
        User loggedInUser = userRepository.findByUserId(userId);
        if (loggedInUser == null) {
            return Page.empty();
        }

        PageRequest pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "purchaseDate"));

        if (loggedInUser.getRole() == RoleType.SuperAdmin) {
            return equipmentRepository.findAll(pageable);
        }

        List<Level> scopedLevels = levelService.getAllCellsUnder(loggedInUser.getLevel());

        if (loggedInUser.getLevel().getLevelType() == LevelType.CELL &&
                !scopedLevels.contains(loggedInUser.getLevel())) {
            scopedLevels.add(loggedInUser.getLevel());
        }

        return equipmentRepository.findByLevelIn(scopedLevels, pageable);
    }

    // Equipment statistics with proper level-based counting
    public EquipmentStatsDTO getScopedEquipmentStats(String userId) {
        User loggedInUser = userRepository.findByUserId(userId);
        if (loggedInUser == null) {
            return new EquipmentStatsDTO(0, 0, 0, 0, 0);
        }

        long totalEquipment;
        long excellentCount;
        long goodCount;
        long needsRepairCount;
        long outOfServiceCount;

        if (loggedInUser.getRole() == RoleType.SuperAdmin) {
            totalEquipment = equipmentRepository.count();
            excellentCount = equipmentRepository.countByCondition("Excellent");
            goodCount = equipmentRepository.countByCondition("Good");
            needsRepairCount = equipmentRepository.countByCondition("Needs Repair");
            outOfServiceCount = equipmentRepository.countByCondition("Out of Service");
        } else {
            // Use scoped CELL levels (DBRef Level objects)
            List<Level> scopedCells = levelService.getAllCellsUnder(loggedInUser.getLevel());

            if (loggedInUser.getLevel().getLevelType() == LevelType.CELL &&
                    !scopedCells.contains(loggedInUser.getLevel())) {
                scopedCells.add(loggedInUser.getLevel());
            }

            //  Count using DBRef Level objects
            totalEquipment = equipmentRepository.countByLevelIn(scopedCells);
            excellentCount = equipmentRepository.countByConditionAndLevelIn("Excellent", scopedCells);
            goodCount = equipmentRepository.countByConditionAndLevelIn("Good", scopedCells);
            needsRepairCount = equipmentRepository.countByConditionAndLevelIn("Needs Repair", scopedCells);
            outOfServiceCount = equipmentRepository.countByConditionAndLevelIn("Out of Service", scopedCells);
        }

        return new EquipmentStatsDTO(totalEquipment, excellentCount, goodCount, needsRepairCount, outOfServiceCount);
    }
}