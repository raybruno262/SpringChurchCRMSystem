package com.SpringChurchCRMSystem.SpringChurchCRMSystem.controller;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.service.LevelService;

import io.swagger.v3.oas.annotations.parameters.RequestBody;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;

@RestController

@RequestMapping("api/levels")
public class LevelController {
    @Autowired
    private LevelService levelService;

    // creating all levels
    @PostMapping("/createAllLevels")
    public String createAllLevels(@RequestParam String headquarterName, @RequestParam String headquarterAddress,
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
    public String addOneLevel(@RequestParam String levelName, @RequestParam String levelAddress,
            @RequestParam String parentId) {
        return levelService.addOneLevel(levelName, levelAddress, parentId);

    }

    // getting all levels
    @GetMapping("/allLevels")
    public List<Level> allLevels() {
        return levelService.findAll();
    }

    // get all paginated levels
    @GetMapping("/paginatedLevels")
    public Page<Level> getPaginatedLevels(@RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {
        return levelService.getPaginatedLevels(page, size);

    }

    // get children of a level
    @GetMapping("/allChildren")
    public List<Level> allChildren(@RequestParam String parentId) {
        return levelService.getChildren(parentId);
    }

    // get all Descendants of a level
    @GetMapping("/allDescendants")
    public List<Level> allDescendants(@RequestParam String parentId) {
        return levelService.getAllDescendants(parentId);
    }

    // get level by ID
    @GetMapping("/getLevelById")
    public Optional<Level> getLevelById(@RequestParam String levelId) {
        return levelService.getLevelById(levelId);
    }

}
