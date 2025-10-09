package com.SpringChurchCRMSystem.SpringChurchCRMSystem.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.CustomGlobalExceptionHandler;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.LevelType;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.RoleType;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.User;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.LevelRepository;

import jakarta.servlet.http.HttpSession;

@Service
public class LevelService {

    @Autowired
    private LevelRepository levelRepository;
    @Autowired
    private HttpSession userSession;

    // Creating all levels at once
    public ResponseEntity<String> createAllLevels(String headquarterName, String headquarterAddress,
            String regionName, String regionAddress,
            String parishName, String parishAddress,
            String chapelName, String chapelAddress,
            String cellName, String cellAddress) {
        try {
            User loggedInUser = (User) userSession.getAttribute("loggedInUser");

            // No user logged in
            if (loggedInUser == null) {
                return ResponseEntity.ok("Status 4000");
            }

            // Not a SuperAdmin
            if (loggedInUser.getRole() != RoleType.SuperAdmin) {
                return ResponseEntity.ok("Status 6000");
            }

            // HEADQUARTER (always required)
            Optional<Level> existingHeadquarter = levelRepository.findByNameAndLevelType(headquarterName,
                    LevelType.HEADQUARTER);
            Level headquarter = existingHeadquarter.orElseGet(() -> {
                Level hq = new Level();
                hq.setName(headquarterName);
                hq.setAddress(headquarterAddress);
                hq.setLevelType(LevelType.HEADQUARTER);
                hq.setIsActive(true);
                return levelRepository.save(hq);
            });

            Level currentParent = headquarter;

            // REGION (optional)
            if (regionName != null && regionAddress != null) {
                Level region = new Level();
                region.setName(regionName);
                region.setAddress(regionAddress);
                region.setLevelType(LevelType.REGION);
                region.setIsActive(true);
                region.setParent(currentParent);
                region = levelRepository.save(region);
                currentParent = region;
            }

            // PARISH (optional)
            if (parishName != null && parishAddress != null) {
                Level parish = new Level();
                parish.setName(parishName);
                parish.setAddress(parishAddress);
                parish.setLevelType(LevelType.PARISH);
                parish.setIsActive(true);
                parish.setParent(currentParent);
                parish = levelRepository.save(parish);
                currentParent = parish;
            }

            // CHAPEL (optional)
            if (chapelName != null && chapelAddress != null) {
                Level chapel = new Level();
                chapel.setName(chapelName);
                chapel.setAddress(chapelAddress);
                chapel.setLevelType(LevelType.CHAPEL);
                chapel.setIsActive(true);
                chapel.setParent(currentParent);
                chapel = levelRepository.save(chapel);
                currentParent = chapel;
            }

            // CELL (optional)
            if (cellName != null && cellAddress != null) {
                Level cell = new Level();
                cell.setName(cellName);
                cell.setAddress(cellAddress);
                cell.setLevelType(LevelType.CELL);
                cell.setIsActive(true);
                cell.setParent(currentParent);
                levelRepository.save(cell);
            }

            return ResponseEntity.ok("Status 1000"); // Success

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 9999"); // Unknown error
        }
    }

    // create a single level from an existing parent
    public ResponseEntity<String> addOneLevel(String levelName, String levelAddress, String parentId) {
        try {
            User loggedInUser = (User) userSession.getAttribute("loggedInUser");

            // No user logged in
            if (loggedInUser == null) {
                return ResponseEntity.ok("Status 4000");
            }

            // Not a SuperAdmin
            if (loggedInUser.getRole() != RoleType.SuperAdmin) {
                return ResponseEntity.ok("Status 6000");
            }
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
        return levelRepository.findAll();
    }

    // Get all Active Levels
    public List<Level> getAllActiveLevels() {
        return levelRepository.findByIsActiveTrue();
    }

    // Get all inactive levels
    public List<Level> getAllInactiveLevels() {
        return levelRepository.findByIsActiveFalse();
    }

    // Getting all paginated levels
    public Page<Level> getPaginatedLevels(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size);
        return levelRepository.findAll(pageable);
    }

    // Getting all paginated Active levels
    public Page<Level> getPaginatedActiveLevels(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size);
        return levelRepository.findByIsActiveTrue(pageable);
    }

    // Getting all paginated Inactive levels
    public Page<Level> getPaginatedInactiveLevels(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size);
        return levelRepository.findByIsActiveFalse(pageable);
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

    // Find all Descendants for all Active levels
    public List<Level> getActiveDescendants(String parentId) {
        List<Level> allLevels = levelRepository.findByIsActiveTrue();
        List<Level> activeDescendants = new ArrayList<>();
        findDescendants(parentId, allLevels, activeDescendants);
        return activeDescendants;
    }

    // Find all Descendants for all InActive levels
    public List<Level> getInactiveDescendants(String parentId) {
        List<Level> allLevels = levelRepository.findByIsActiveFalse();
        List<Level> inactiveDescendants = new ArrayList<>();
        findDescendants(parentId, allLevels, inactiveDescendants);
        return inactiveDescendants;
    }

    // find level by Id
    public Optional<Level> getLevelById(String levelId) {
        return levelRepository.findById(levelId);
    }

    // Get all cells under each level
    public List<Level> getAllCellsUnder(Level level) {
        List<Level> children = levelRepository.findByParent(level);
        List<Level> cells = new ArrayList<>();

        for (Level child : children) {
            if (child.getLevelType() == LevelType.CELL) {
                cells.add(child);
            } else {
                cells.addAll(getAllCellsUnder(child));
            }
        }
        return cells;
    }

    // Get All Active Cells Under a Level
    public List<Level> getAllActiveCellsUnder(Level level) {
        List<Level> children = levelRepository.findByParent(level);
        List<Level> activeCells = new ArrayList<>();

        for (Level child : children) {
            if (child.getLevelType() == LevelType.CELL && child.getIsActive()) {
                activeCells.add(child);
            } else {
                activeCells.addAll(getAllActiveCellsUnder(child));
            }
        }
        return activeCells;
    }

    // Get All inactive cells under a level
    public List<Level> getAllInactiveCellsUnder(Level level) {
        List<Level> children = levelRepository.findByParent(level);
        List<Level> inactiveCells = new ArrayList<>();

        for (Level child : children) {
            if (child.getLevelType() == LevelType.CELL && !child.getIsActive()) {
                inactiveCells.add(child);
            } else {
                inactiveCells.addAll(getAllInactiveCellsUnder(child));
            }
        }
        return inactiveCells;
    }

    // Update level
    public ResponseEntity<String> updateLevel(String levelId, Level updatedData) {
        try {
            User loggedInUser = (User) userSession.getAttribute("loggedInUser");

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
        List<Level> children = levelRepository.findByParent(level);
        List<Level> allLevels = new ArrayList<>();

        for (Level child : children) {
            allLevels.add(child); // include every child
            allLevels.addAll(getAllLevelsUnder(child)); // recurse
        }

        return allLevels;
    }

}
