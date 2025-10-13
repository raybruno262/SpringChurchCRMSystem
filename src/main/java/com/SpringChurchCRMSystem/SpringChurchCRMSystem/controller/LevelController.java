package com.SpringChurchCRMSystem.SpringChurchCRMSystem.controller;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
            @RequestParam(required = false) String cellAddress, @PathVariable String userId) {

        return levelService.createAllLevels(
                headquarterName, headquarterAddress,
                regionName, regionAddress,
                parishName, parishAddress,
                chapelName, chapelAddress,
                cellName, cellAddress, userId);
    }

    // addd one level from an existing parent
    @PostMapping("/addOneLevel")
    public ResponseEntity<String> addOneLevel(@RequestParam String levelName, @RequestParam String levelAddress,
            @RequestParam String parentId) {

        return levelService.addOneLevel(levelName, levelAddress, parentId);

    }

    // Update level
    @PutMapping("/updateLevel/{levelId}")
    public ResponseEntity<String> updateLevel(@PathVariable String levelId, @RequestBody Level updatedData) {

        return levelService.updateLevel(levelId, updatedData);

    }

    // getting all levels
    @GetMapping("/getAllLevels")
    public List<Level> getAllLevels() {

        return levelService.getAllLevels();

    }

    // get all paginated levels
    @GetMapping("/paginatedLevels")
    public Page<Level> getPaginatedLevels(@RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {

        return levelService.getPaginatedLevels(page, size);

    }

    // get all Descendants of a level
    @GetMapping("/getAllDescendants")
    public List<Level> getAllDescendants(@RequestParam String parentId) {

        return levelService.getAllDescendants(parentId);

    }

    // get level by ID
    @GetMapping("/getLevelById")
    public Optional<Level> getLevelById(@RequestParam String levelId) {

        return levelService.getLevelById(levelId);

    }

    // get level counts by level types
    @GetMapping("/levelCounts")
    public Map<String, Integer> getLevelCounts() {
        Map<String, Integer> counts = new HashMap<>();
        counts.put("regions", levelService.countByType("REGION"));
        counts.put("parishes", levelService.countByType("PARISH"));
        counts.put("chapels", levelService.countByType("CHAPEL"));
        counts.put("cells", levelService.countByType("CELL"));
        return counts;
    }

    // get all cells
    @GetMapping("allCells")
    public List<Level> getAllCells() {
        return levelService.getAllCells();
    }

}
