package com.SpringChurchCRMSystem.SpringChurchCRMSystem.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.CustomGlobalExceptionHandler;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.LevelType;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.RoleType;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.User;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.LevelRepository;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.UserRepository;

import jakarta.servlet.http.HttpSession;

@Service
public class LevelService {

    @Autowired
    private LevelRepository levelRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private HttpSession userSession;

    @PostMapping("/createAllLevels/{userId}")
    public ResponseEntity<String> createAllLevels(
            @RequestParam(required = false) String headquarterName,
            @RequestParam(required = false) String headquarterAddress,
            @RequestParam(required = false) String regionName,
            @RequestParam(required = false) String regionAddress,
            @RequestParam(required = false) String parishName,
            @RequestParam(required = false) String parishAddress,
            @RequestParam(required = false) String chapelName,
            @RequestParam(required = false) String chapelAddress,
            @RequestParam(required = false) String cellName,
            @RequestParam(required = false) String cellAddress,
            @PathVariable String userId) {

        try {
            User loggedInUser = userRepository.findByUserId(userId);
            if (loggedInUser == null)
                return ResponseEntity.ok("Status 4000");

            if (loggedInUser.getRole() != RoleType.SuperAdmin)
                return ResponseEntity.ok("Status 6000");

            Level headquarter;

            boolean wantsToCreateHeadquarter = headquarterName != null && !headquarterName.trim().isEmpty()
                    && headquarterAddress != null && !headquarterAddress.trim().isEmpty();

            Optional<Level> existingHeadquarter = levelRepository.findFirstByLevelType(LevelType.HEADQUARTER);

            if (wantsToCreateHeadquarter) {
                if (existingHeadquarter.isPresent()) {

                    return ResponseEntity.ok("Status 3000");
                }

                headquarter = new Level();
                headquarter.setName(headquarterName.trim());
                headquarter.setAddress(headquarterAddress.trim());
                headquarter.setLevelType(LevelType.HEADQUARTER);
                headquarter.setIsActive(true);
                headquarter = levelRepository.save(headquarter);
            } else {
                if (existingHeadquarter.isPresent()) {
                    headquarter = existingHeadquarter.get();
                } else {
                    return ResponseEntity.ok("Status 3000");
                }
            }

            Level currentParent = headquarter;

            if (regionName != null && !regionName.trim().isEmpty() &&
                    regionAddress != null && !regionAddress.trim().isEmpty()) {

                Level region = new Level();
                region.setName(regionName.trim());
                region.setAddress(regionAddress.trim());
                region.setLevelType(LevelType.REGION);
                region.setIsActive(true);
                region.setParent(currentParent);
                region = levelRepository.save(region);
                currentParent = region;
            }

            if (parishName != null && !parishName.trim().isEmpty() &&
                    parishAddress != null && !parishAddress.trim().isEmpty()) {

                Level parish = new Level();
                parish.setName(parishName.trim());
                parish.setAddress(parishAddress.trim());
                parish.setLevelType(LevelType.PARISH);
                parish.setIsActive(true);
                parish.setParent(currentParent);
                parish = levelRepository.save(parish);
                currentParent = parish;
            }

            if (chapelName != null && !chapelName.trim().isEmpty() &&
                    chapelAddress != null && !chapelAddress.trim().isEmpty()) {

                Level chapel = new Level();
                chapel.setName(chapelName.trim());
                chapel.setAddress(chapelAddress.trim());
                chapel.setLevelType(LevelType.CHAPEL);
                chapel.setIsActive(true);
                chapel.setParent(currentParent);
                chapel = levelRepository.save(chapel);
                currentParent = chapel;
            }

            if (cellName != null && !cellName.trim().isEmpty() &&
                    cellAddress != null && !cellAddress.trim().isEmpty()) {

                Level cell = new Level();
                cell.setName(cellName.trim());
                cell.setAddress(cellAddress.trim());
                cell.setLevelType(LevelType.CELL);
                cell.setIsActive(true);
                cell.setParent(currentParent);
                levelRepository.save(cell);
            }

            return ResponseEntity.ok("Status 1000");

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 9999");
        }
    }

    // create a single level from an existing parent
    public ResponseEntity<String> addOneLevel(String levelName, String levelAddress, String parentId,
            String userId) {
        try {
            User loggedInUser = userRepository.findByUserId(userId);
            if (loggedInUser == null)
                return ResponseEntity.ok("Status 4000");
            if (loggedInUser.getRole() != RoleType.SuperAdmin)
                return ResponseEntity.ok("Status 6000");
            // Validate input
            if (levelName == null || levelAddress == null) {
                return ResponseEntity.ok("Status 3000"); // Missing required fields
            }

            Optional<Level> existingParent = levelRepository.findById(parentId);
            if (existingParent.isEmpty()) {
                return ResponseEntity.ok("Status 3000"); // Parent not found
            }

            Level parent = existingParent.get();
            LevelType levelType;

            switch (parent.getLevelType()) {
                case HEADQUARTER -> levelType = LevelType.REGION;
                case REGION -> levelType = LevelType.PARISH;
                case PARISH -> levelType = LevelType.CHAPEL;
                case CHAPEL -> levelType = LevelType.CELL;
                default -> {
                    return ResponseEntity.ok("Status 3000"); // Invalid parent type
                }
            }

            Level newLevel = new Level();
            newLevel.setName(levelName);
            newLevel.setAddress(levelAddress);
            newLevel.setLevelType(levelType);
            newLevel.setIsActive(true);
            newLevel.setParent(parent);
            levelRepository.save(newLevel);

            return ResponseEntity.ok("Status 1000"); // Success

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 9999"); // Unknown error
        }
    }

    // Get all levels regardless of status
    public List<Level> getAllLevels() {
        return levelRepository.findAll(Sort.by(Sort.Direction.DESC, "levelId"));
    }

    // find all cells
    public List<Level> getAllCells() {
        return levelRepository.findByLevelType(LevelType.CELL);
    }

    // Getting all paginated levels
    public Page<Level> getPaginatedLevels(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "levelId"));
        return levelRepository.findAll(pageable);
    }

    // Find Descendants void method
    private void findDescendants(String parentId, List<Level> allLevels, List<Level> result) {
        for (Level level : allLevels) {
            if (level.getParent() != null && level.getParent().getLevelId().equals(parentId)) {
                result.add(level);
                findDescendants(level.getLevelId(), allLevels, result);
            }
        }
    }

    // Find all Descendants for all levels
    public List<Level> getAllDescendants(String parentId) {
        List<Level> allLevels = levelRepository.findAll();
        List<Level> descendants = new ArrayList<>();
        findDescendants(parentId, allLevels, descendants);
        return descendants;
    }

    // find level by Id
    public Optional<Level> getLevelById(String levelId) {
        return levelRepository.findById(levelId);
    }

    // Update level
    public ResponseEntity<String> updateLevel(String levelId, Level updatedData, String userId) {
        try {
            User loggedInUser = userRepository.findByUserId(userId);
            if (loggedInUser == null)
                return ResponseEntity.ok("Status 4000");

            if (loggedInUser.getRole() != RoleType.SuperAdmin)
                return ResponseEntity.ok("Status 6000");

            Optional<Level> levelOpt = levelRepository.findById(levelId);
            if (levelOpt.isEmpty())
                return ResponseEntity.ok("Status 3000");

            Level level = levelOpt.get();

            // 1️⃣ Update name/address
            if (updatedData.getName() != null && !updatedData.getName().isBlank()) {
                level.setName(updatedData.getName());
            }
            if (updatedData.getAddress() != null && !updatedData.getAddress().isBlank()) {
                level.setAddress(updatedData.getAddress());
            }
            if (updatedData.getLevelType() != null) {
                return ResponseEntity.ok("Status 3000");
            }

            // 2️⃣ Reassign parent
            if (updatedData.getParent() != null && updatedData.getParent().getLevelId() != null) {
                String newParentId = updatedData.getParent().getLevelId();
                Optional<Level> newParentOpt = levelRepository.findById(newParentId);
                if (newParentOpt.isEmpty())
                    return ResponseEntity.ok("Status 3000");

                Level newParent = newParentOpt.get();

                if (!level.getIsActive() || !newParent.getIsActive()) {
                    return ResponseEntity.ok(CustomGlobalExceptionHandler.BLOCKED_BY_INACTIVE_ANCESTOR);
                }

                Level oldParent = level.getParent();
                if (oldParent != null && oldParent.getLevelType() != newParent.getLevelType()) {
                    return ResponseEntity.ok("Status 3000");
                }

                level.setParent(newParent);
            }

            // Enabling Level
            if (Boolean.TRUE.equals(updatedData.getIsActive())) {

                if (!hasActiveAncestor(level)) {
                    return ResponseEntity.ok(CustomGlobalExceptionHandler.BLOCKED_BY_INACTIVE_ANCESTOR);
                }

                level.setIsActive(true);
                levelRepository.save(level);

                List<Level> inactiveLevels = levelRepository.findByIsActiveFalse();
                List<Level> descendants = new ArrayList<>();
                findDescendants(levelId, inactiveLevels, descendants);

                for (Level descendant : descendants) {
                    descendant.setIsActive(true);
                }
                levelRepository.saveAll(descendants);

                // Disable Level
            } else if (Boolean.FALSE.equals(updatedData.getIsActive())) {

                level.setIsActive(false);
                levelRepository.save(level);

                List<Level> activeLevels = levelRepository.findByIsActiveTrue();
                List<Level> descendants = new ArrayList<>();
                findDescendants(levelId, activeLevels, descendants);

                for (Level descendant : descendants) {
                    descendant.setIsActive(false);
                }
                levelRepository.saveAll(descendants);
            }

            // Final save if only name/address/parent changed
            if (levelRepository.findById(levelId).isPresent()) {
                levelRepository.save(level);
            }

            return ResponseEntity.ok("Status 1000");

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 9999");
        }
    }

    // Check for Active Ancestor before Enabling a level
    private boolean hasActiveAncestor(Level level) {
        Level current = level;

        while (current.getParent() != null) {
            Level parent = current.getParent();

            // Defensive check: ensure parent reference is valid
            Optional<Level> parentOpt = levelRepository.findById(parent.getLevelId());
            if (parentOpt.isEmpty()) {
                return false;
            }

            Level parentLevel = parentOpt.get();
            if (!parentLevel.getIsActive()) {
                return false; // Inactive ancestor,block enable
            }

            current = parentLevel; // Move up the chain
        }

        return true; // Reached top safely
    }

    // get total level counts using level types
    public int countByType(String type) {
        return levelRepository.countByLevelType(type);
    }

    // get all levels under a level
    public List<Level> getAllLevelsUnder(Level level) {
        List<Level> allLevels = new ArrayList<>();
        allLevels.add(level);

        List<Level> children = levelRepository.findByParent(level);
        for (Level child : children) {
            allLevels.addAll(getAllLevelsUnder(child));
        }

        return allLevels;
    }

    // get all cells under
    public List<Level> getAllCellsUnder(Level level) {
        List<Level> allLevels = getAllLevelsUnder(level);

        List<Level> cells = allLevels.stream()
                .filter(l -> l.getLevelType() == LevelType.CELL)
                .collect(Collectors.toList());

        if (level.getLevelType() == LevelType.CELL && !cells.contains(level)) {
            cells.add(level);
        }

        return cells;
    }

}
