package com.SpringChurchCRMSystem.SpringChurchCRMSystem.controller;

import org.springframework.web.bind.annotation.*;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.Dto.FinanceStatsDTO;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Finance;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.service.FinanceService;

@RestController
@RequestMapping("/api/finance")
public class FinanceController {

    @Autowired
    private FinanceService financeService;

    // Create a finance record
    @PostMapping("/createFinance/{userId}")
    public ResponseEntity<String> createFinance(
            @RequestBody Finance finance,
            @PathVariable String userId) {
        return financeService.createFinance(finance, userId);
    }

    // Get all finance records
    @GetMapping("/allFinance")
    public List<Finance> getAllFinanceRecords() {
        return financeService.getAllFinanceRecords();
    }

    // Get finance record by ID
    @GetMapping("/{financeId}")
    public ResponseEntity<Finance> getFinanceById(@PathVariable String financeId) {
        Finance finance = financeService.getFinanceById(financeId);
        if (finance != null) {
            return ResponseEntity.ok(finance);
        } else {
            return ResponseEntity.notFound().build();
        }
    }

    // Update finance record
    @PostMapping("/updateFinance/{financeId}/{userId}")
    public ResponseEntity<String> updateFinance(
            @PathVariable String financeId,
            @PathVariable String userId,
            @RequestBody Finance updatedData) {
        return financeService.updateFinance(financeId, updatedData, userId);
    }

    // Get paginated finance records (all records)
    @GetMapping("/paginatedFinance")
    public Page<Finance> getPaginatedFinanceRecords(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {
        return financeService.getPaginatedFinanceRecords(page, size);
    }

    // Get scoped paginated finance records based on user hierarchy
    @GetMapping("/scopedPaginatedFinance")
    public Page<Finance> getScopedPaginatedFinanceRecords(
            @RequestParam String userId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {
        return financeService.getScopedPaginatedFinanceRecords(page, size, userId);
    }

    // Get finance statistics
    @GetMapping("/stats")
    public FinanceStatsDTO getScopedFinanceStats(@RequestParam String userId) {
        return financeService.getScopedFinanceStats(userId);
    }
}