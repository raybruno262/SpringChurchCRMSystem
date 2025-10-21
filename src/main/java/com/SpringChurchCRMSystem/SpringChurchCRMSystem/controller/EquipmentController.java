package com.SpringChurchCRMSystem.SpringChurchCRMSystem.controller;

import org.springframework.web.bind.annotation.*;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.Dto.EquipmentStatsDTO;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Equipment;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.service.EquipmentService;

@RestController
@RequestMapping("/api/equipment")
public class EquipmentController {

    @Autowired
    private EquipmentService equipmentService;

    // Create equipment
    @PostMapping("/createEquipment/{userId}")
    public ResponseEntity<String> createEquipment(
            @RequestBody Equipment equipment,
            @PathVariable String userId) {
        return equipmentService.createEquipment(equipment, userId);
    }

    // Get all equipment records
    @GetMapping("/allEquipment")
    public List<Equipment> getAllEquipment() {
        return equipmentService.getAllEquipment();
    }

    // Get equipment by ID
    @GetMapping("/{equipmentId}")
    public ResponseEntity<Equipment> getEquipmentById(@PathVariable String equipmentId) {
        Equipment equipment = equipmentService.getEquipmentById(equipmentId);
        if (equipment != null) {
            return ResponseEntity.ok(equipment);
        } else {
            return ResponseEntity.notFound().build();
        }
    }

    // Update equipment record
    @PostMapping("/updateEquipment/{equipmentId}/{userId}")
    public ResponseEntity<String> updateEquipment(
            @PathVariable String equipmentId,
            @PathVariable String userId,
            @RequestBody Equipment updatedData) {
        return equipmentService.updateEquipment(equipmentId, updatedData, userId);
    }

    // Get paginated equipment records (all records)
    @GetMapping("/paginatedEquipment")
    public Page<Equipment> getPaginatedEquipment(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {
        return equipmentService.getPaginatedEquipment(page, size);
    }

    // Get scoped paginated equipment records based on user hierarchy
    @GetMapping("/scopedPaginatedEquipment")
    public Page<Equipment> getScopedPaginatedEquipment(
            @RequestParam String userId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {
        return equipmentService.getScopedPaginatedEquipment(page, size, userId);
    }

    // Get equipment statistics
    @GetMapping("/stats")
    public EquipmentStatsDTO getScopedEquipmentStats(@RequestParam String userId) {
        return equipmentService.getScopedEquipmentStats(userId);
    }
}