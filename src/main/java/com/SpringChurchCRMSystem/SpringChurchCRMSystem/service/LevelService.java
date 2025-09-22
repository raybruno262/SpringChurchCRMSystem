package com.SpringChurchCRMSystem.SpringChurchCRMSystem.service;

import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
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

            default:
                return "Level type not found";

        }

        Level newLevel = new Level();
        newLevel.setName(levelName);
        newLevel.setAddress(levelAddress);
        newLevel.setLevelType(levelType);
        newLevel.setParent(parent);
        levelRepository.save(newLevel);

        return "Level saved Successfully";

    }

}
