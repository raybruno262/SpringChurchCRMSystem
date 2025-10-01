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
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.LevelRepository;

@Service
public class LevelService {

    @Autowired
    private LevelRepository levelRepository;

    // Creating all levels at once
    public ResponseEntity<String> createAllLevels(String headquarterName, String headquarterAddress,
            String regionName, String regionAddress,
            String parishName, String parishAddress,
            String chapelName, String chapelAddress,
            String cellName, String cellAddress) {
        try {
            // HEADQUARTER (always required)
            Optional<Level> existingHeadquarter = levelRepository.findByNameAndLevelType(headquarterName,
                    LevelType.HEADQUARTER);
            Level headquarter = existingHeadquarter.orElseGet(() -> {
                Level hq = new Level();
                hq.setName(headquarterName);
                hq.setAddress(headquarterAddress);
                hq.setLevelType(LevelType.HEADQUARTER);
                return levelRepository.save(hq);
            });

            Level currentParent = headquarter;

            // REGION (optional)
            if (regionName != null && regionAddress != null) {
                Level region = new Level();
                region.setName(regionName);
                region.setAddress(regionAddress);
                region.setLevelType(LevelType.REGION);
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
            if (child.getLevelType() == LevelType.CELL && child.isActive()) {
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
            if (child.getLevelType() == LevelType.CELL && !child.isActive()) {
                inactiveCells.add(child);
            } else {
                inactiveCells.addAll(getAllInactiveCellsUnder(child));
            }
        }
        return inactiveCells;
    }

    // Disable all the active level and its Descendants
    public ResponseEntity<String> disableLevelAndDescendants(String levelId) {
        Optional<Level> rootOpt = levelRepository.findById(levelId);
        try {
            if (rootOpt.isPresent()) {
                Level rootLevel = rootOpt.get();
                rootLevel.setActive(false);
                levelRepository.save(rootLevel);

                List<Level> allLevels = levelRepository.findAll();
                List<Level> descendants = new ArrayList<>();
                findDescendants(levelId, allLevels, descendants);

                for (Level descendant : descendants) {
                    descendant.setActive(false);
                }

                levelRepository.saveAll(descendants);

            }
            return ResponseEntity.ok("Status 1000"); // Success

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 9999"); // Unknown error
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
            if (!parentLevel.isActive()) {
                return false; // Inactive ancestor,block enable
            }

            current = parentLevel; // Move up the chain
        }

        return true; // Reached top safely
    }

    // Enable(Restore) a level
    public ResponseEntity<String> enableLevelAndDescendants(String levelId) {
        try {
            Optional<Level> rootOpt = levelRepository.findById(levelId);

            if (rootOpt.isEmpty()) {
                return ResponseEntity.ok("Status 3000"); // Level not found
            }

            Level rootLevel = rootOpt.get();

            // Recursively check ancestor chain
            if (!hasActiveAncestor(rootLevel)) {
                // blocked by inactive ancestor
                return ResponseEntity.ok(CustomGlobalExceptionHandler.BLOCKED_BY_INACTIVE_ANCESTOR);

            }

            // Enable root level
            rootLevel.setActive(true);
            levelRepository.save(rootLevel);

            // Enable all descendants
            List<Level> inactiveLevels = levelRepository.findByIsActiveFalse();
            List<Level> descendants = new ArrayList<>();
            findDescendants(levelId, inactiveLevels, descendants);

            for (Level descendant : descendants) {
                descendant.setActive(true);
            }

            levelRepository.saveAll(descendants);

            return ResponseEntity.ok("Status 1000"); // Success

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 9999"); // Unknown error
        }
    }

    // Update a single Level's name or Address or both
    public ResponseEntity<String> updateLevelDetails(String levelId, Level updatedData) {
        try {
            Optional<Level> levelOpt = levelRepository.findById(levelId);
            if (levelOpt.isEmpty()) {
                return ResponseEntity.ok("Status 3000"); // Level not found
            }

            Level level = levelOpt.get();

            // Update name if provided
            if (updatedData.getName() != null && !updatedData.getName().isBlank()) {
                level.setName(updatedData.getName());
            }

            // Update address if provided
            if (updatedData.getAddress() != null && !updatedData.getAddress().isBlank()) {
                level.setAddress(updatedData.getAddress());
            }

            levelRepository.save(level);
            return ResponseEntity.ok("Status 1000"); // Success

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 9999"); // Unknown error
        }
    }

    // Update a parentId where old parent roletype == new parent roletype
    public ResponseEntity<String> reassignLevelParent(String levelId, String newParentId) {
        try {
            Optional<Level> levelOpt = levelRepository.findById(levelId);
            Optional<Level> newParentOpt = levelRepository.findById(newParentId);

            if (levelOpt.isEmpty() || newParentOpt.isEmpty()) {
                return ResponseEntity.ok("Status 3000"); // Not found
            }

            Level level = levelOpt.get();
            Level newParent = newParentOpt.get();

            // Can not reassign if either is inactive
            if (!level.isActive() || !newParent.isActive()) {

                return ResponseEntity.ok(CustomGlobalExceptionHandler.BLOCKED_BY_INACTIVE_ANCESTOR);
            }

            // Check if old parent type matches new parent type
            Level oldParent = level.getParent();
            if (oldParent != null && oldParent.getLevelType() != newParent.getLevelType()) {
                return ResponseEntity.ok("Status 3000"); // RoleTypes must be the same
            }

            level.setParent(newParent);
            levelRepository.save(level);

            return ResponseEntity.ok("Status 1000"); // Success

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 9999"); // Unknown error
        }
    }

}
