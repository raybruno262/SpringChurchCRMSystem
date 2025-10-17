package com.SpringChurchCRMSystem.SpringChurchCRMSystem.controller;

import org.springframework.web.bind.annotation.*;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.Dto.VisitorStatsDTO;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Visitor;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.service.VisitorService;

@RestController
@RequestMapping("/api/visitors")
public class VisitorController {

    @Autowired
    private VisitorService visitorService;

    // Create visitor (form-data)
    @PostMapping("/createVisitor/{userId}")
    public ResponseEntity<String> createVisitor(
            @RequestBody Visitor visitor,
            @PathVariable String userId) {
        return visitorService.createVisitor(visitor, userId);
    }

    // Get all visitors
    @GetMapping("/allVisitors")
    public List<Visitor> getAllVisitors() {
        return visitorService.getAllVisitors();
    }

    // Get visitor by ID
    @GetMapping("/{visitorId}")
    public ResponseEntity<Visitor> getVisitorById(@PathVariable String visitorId) {
        Visitor visitor = visitorService.getVisitorById(visitorId);
        if (visitor != null) {
            return ResponseEntity.ok(visitor);
        } else {
            return ResponseEntity.notFound().build();
        }
    }

    // Update visitor
    @PutMapping("/updateVisitor/{visitorId}/{userId}")
    public ResponseEntity<String> updateVisitor(
            @PathVariable String visitorId,
            @PathVariable String userId,
            @RequestBody Visitor updatedData) {
        return visitorService.updateVisitor(visitorId, updatedData, userId);
    }

    // Paginated visitors
    @GetMapping("/paginatedVisitors")
    public Page<Visitor> getPaginatedVisitors(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {
        return visitorService.getPaginatedVisitors(page, size);
    }

    // Scoped paginated visitors
    @GetMapping("/scopedPaginatedVisitors")
    public Page<Visitor> getScopedPaginatedVisitors(
            @RequestParam String userId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {
        return visitorService.getScopedPaginatedVisitors(page, size, userId);
    }

    // Visitor stats
    @GetMapping("/stats")
    public VisitorStatsDTO getScopedVisitorStats(@RequestParam String userId) {
        return visitorService.getScopedVisitorStats(userId);
    }
}
