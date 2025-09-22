package com.SpringChurchCRMSystem.SpringChurchCRMSystem.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.service.LevelService;

@RestController

@RequestMapping("api/levels")
public class LevelController {
    @Autowired
    private LevelService levelService;

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

    @PostMapping("/addOneLevel")
    public String addOneLevel(@RequestParam String levelName, @RequestParam String levelAddress,
            @RequestParam String parentId) {
        return levelService.addOneLevel(levelName, levelAddress, parentId);

    }
}
