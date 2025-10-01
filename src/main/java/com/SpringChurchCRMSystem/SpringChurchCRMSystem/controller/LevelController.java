package com.SpringChurchCRMSystem.SpringChurchCRMSystem.controller;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.service.LevelService;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;

@RestController

@RequestMapping("api/levels")
public class LevelController {
    @Autowired
    private LevelService levelService;

    // creating all levels
    @PostMapping("/createAllLevels")
    public ResponseEntity<String> createAllLevels(@RequestParam String headquarterName,
            @RequestParam String headquarterAddress,
            @RequestParam String regionName, @RequestParam String regionAddress,
            @RequestParam String parishName, @RequestParam String parishAddress,
            @RequestParam String chapelName, @RequestParam String chapelAddress,
            @RequestParam String cellName, @RequestParam String cellAddress) {
        return levelService.createAllLevels(headquarterName, headquarterAddress, regionName,
                regionAddress, parishName, parishAddress, chapelName, chapelAddress,
                cellName, cellAddress);

    }

    // addd one level from an existing parent
    @PostMapping("/addOneLevel")
    public ResponseEntity<String> addOneLevel(@RequestParam String levelName, @RequestParam String levelAddress,
            @RequestParam String parentId) {
        return levelService.addOneLevel(levelName, levelAddress, parentId);

    }

    // getting all levels
    @GetMapping("/getAllLevels")
    public List<Level> getAllLevels() {
        return levelService.getAllLevels();
    }

    // getting all Active levels
    @GetMapping("/getAllActiveLevels")
    public List<Level> getAllActiveLevels() {
        return levelService.getAllActiveLevels();
    }

    // getting all Inactive levels
    @GetMapping("/getAllInactiveLevels")
    public List<Level> getAllInactiveLevels() {
        return levelService.getAllInactiveLevels();
    }

    // get all paginated levels
    @GetMapping("/paginatedLevels")
    public Page<Level> getPaginatedLevels(@RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {
        return levelService.getPaginatedLevels(page, size);

    }

    // get all paginated Active levels
    @GetMapping("/getPaginatedActiveLevels")
    public Page<Level> getPaginatedActiveLevels(@RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {
        return levelService.getPaginatedActiveLevels(page, size);

    }

    // get all paginated Inactivelevels
    @GetMapping("/getPaginatedInactiveLevels")
    public Page<Level> getPaginatedInactiveLevels(@RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {
        return levelService.getPaginatedInactiveLevels(page, size);

    }

    // get all Descendants of a level
    @GetMapping("/getAllDescendants")
    public List<Level> getAllDescendants(@RequestParam String parentId) {
        return levelService.getAllDescendants(parentId);
    }

    // get all Descendants of Active level
    @GetMapping("/getActiveDescendants")
    public List<Level> getActiveDescendants(@RequestParam String parentId) {
        return levelService.getActiveDescendants(parentId);
    }

    // get all Descendants of InActive level
    @GetMapping("/getInactiveDescendants")
    public List<Level> getInactiveDescendants(@RequestParam String parentId) {
        return levelService.getInactiveDescendants(parentId);
    }

    // get level by ID
    @GetMapping("/getLevelById")
    public Optional<Level> getLevelById(@RequestParam String levelId) {
        return levelService.getLevelById(levelId);
    }

    // Disable all the active level and its Descendants
    @PutMapping("/disableLevelAndDescendants")
    public ResponseEntity<String> disableLevelAndDescendants(@RequestParam String levelId) {
        return levelService.disableLevelAndDescendants(levelId);
    }

    // Enable all the Inactive level and its Descendants
    @PutMapping("/enableLevelAndDescendants")
    public ResponseEntity<String> enableLevelAndDescendants(@RequestParam String levelId) {
        return levelService.enableLevelAndDescendants(levelId);
    }

    @PutMapping("/updateLevelNameorAddress/{levelId}")
    public ResponseEntity<String> updateLevelNameorAddress(@PathVariable String levelId,
            @RequestBody Level updatedData) {
        return levelService.updateLevelDetails(levelId, updatedData);
    }

    @PutMapping("/reassignLevelParent")
    public ResponseEntity<String> updateLevelNameorAddress(@RequestParam String levelId,
            @RequestParam String newParentId) {
        return levelService.reassignLevelParent(levelId, newParentId);
    }

}
