package com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Finance;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;

public interface FinanceRepository extends MongoRepository<Finance, String> {
    List<Finance> findByLevelIn(List<Level> levels);

    Page<Finance> findByLevelIn(List<Level> levels, Pageable pageable);

    Double sumAmountByTransactionType(String transactionType);
}
