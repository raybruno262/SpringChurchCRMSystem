package com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository;

import java.util.List;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;

public interface FinanceRepositoryCustom {
    Double sumAmountByTransactionType(String transactionType);

    Double sumAmountByTransactionTypeAndLevelIn(String transactionType, List<Level> levels);
}