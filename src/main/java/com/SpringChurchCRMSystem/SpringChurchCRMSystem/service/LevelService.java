package com.SpringChurchCRMSystem.SpringChurchCRMSystem.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.LevelType;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.LevelRepository;

@Service
public class LevelService {

    @Autowired
    private LevelRepository levelRepository;

    // Creating all the levels at once
    public String createAllLevels(String headquarterName, String headquarterAddress,
            String regionName, String regionAddress,
            String parishName, String parishAddress,
            String chapelName, String chapelAddress,
            String cellName, String cellAddress) {

        // HEADQUARTER
        Optional<Level> existingHeadquarter = levelRepository.findByNameAndLevelType(headquarterName,
                LevelType.HEADQUARTER);
        Level headquarter = existingHeadquarter.orElseGet(() -> {
            Level hq = new Level();
            hq.setName(headquarterName);
            hq.setAddress(headquarterAddress);
            hq.setLevelType(LevelType.HEADQUARTER);
            return levelRepository.save(hq);

        });

        // REGION
        Level region = new Level();
        region.setName(regionName);
        region.setAddress(regionAddress);
        region.setLevelType(LevelType.REGION);
        region.setParent(headquarter);
        levelRepository.save(region);

        // PARISH
        Level parish = new Level();
        parish.setName(parishName);
        parish.setAddress(parishAddress);
        parish.setLevelType(LevelType.PARISH);
        parish.setParent(region);
        levelRepository.save(parish);

        // CHAPEL
        Level chapel = new Level();
        chapel.setName(chapelName);
        chapel.setAddress(chapelAddress);
        chapel.setLevelType(LevelType.CHAPEL);
        chapel.setParent(parish);
        levelRepository.save(chapel);

        // CELL
        Level cell = new Level();
        cell.setName(cellName);
        cell.setAddress(cellAddress);
        cell.setLevelType(LevelType.CELL);
        cell.setParent(chapel);
        levelRepository.save(cell);

        return "Levels Created Succefully";

    }

    // create a single level from an existing parent
    public String addOneLevel(String levelName, String levelAddress, String parentId) {
        Optional<Level> existingParent = levelRepository.findById(parentId);
        if (existingParent.isEmpty()) {
            return "Parent Level not Found";
        }

        Level parent = existingParent.get();
        LevelType levelType;
        switch (parent.getLevelType()) {
            case HEADQUARTER:
                levelType = LevelType.REGION;
                break;
            case REGION:
                levelType = LevelType.PARISH;
                break;
            case PARISH:
                levelType = LevelType.CHAPEL;
                break;
            case CHAPEL:
                levelType = LevelType.CELL;
                break;

            default:
                return "Parent Level type not found";

        }

        Level newLevel = new Level();
        newLevel.setName(levelName);
        newLevel.setAddress(levelAddress);
        newLevel.setLevelType(levelType);
        newLevel.setParent(parent);
        levelRepository.save(newLevel);

        return "Level saved Successfully";

    }

    // Getting all levels
    public List<Level> findAll() {
        return levelRepository.findAll();
    }

    // Getting all paginated levels
    public Page<Level> getPaginatedLevels(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size);
        return levelRepository.findAll(pageable);
    }

    // Get only children of a level
    public List<Level> getChildren(String parentId) {
        return levelRepository.findAll().stream()
                .filter(level -> level.getParent() != null && level.getParent().getLevelId().equals(parentId))
                .collect(Collectors.toList());

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

    // Find all Descendants now
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

}
