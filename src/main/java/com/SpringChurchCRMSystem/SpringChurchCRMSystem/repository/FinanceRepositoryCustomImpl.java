package com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository;

import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.aggregation.Aggregation;
import org.springframework.data.mongodb.core.aggregation.AggregationResults;
import org.springframework.data.mongodb.core.aggregation.MatchOperation;
import org.springframework.data.mongodb.core.aggregation.GroupOperation;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.stereotype.Repository;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;

@Repository
public class FinanceRepositoryCustomImpl implements FinanceRepositoryCustom {

    @Autowired
    private MongoTemplate mongoTemplate;

    @Override
    public Double sumAmountByTransactionType(String transactionType) {
        try {
            MatchOperation matchStage = Aggregation.match(Criteria.where("transactionType").is(transactionType));
            GroupOperation groupStage = Aggregation.group().sum("amount").as("totalAmount");

            Aggregation aggregation = Aggregation.newAggregation(matchStage, groupStage);

            AggregationResults<TotalAmount> results = mongoTemplate.aggregate(
                    aggregation, "finance", TotalAmount.class);

            return results.getUniqueMappedResult() != null ? results.getUniqueMappedResult().getTotalAmount() : 0.0;
        } catch (Exception e) {
            e.printStackTrace();
            return 0.0;
        }
    }

    @Override
    public Double sumAmountByTransactionTypeAndLevelIn(String transactionType, List<Level> levels) {
        try {
            MatchOperation matchStage = Aggregation.match(
                    Criteria.where("transactionType").is(transactionType)
                            .and("level").in(levels));
            GroupOperation groupStage = Aggregation.group().sum("amount").as("totalAmount");

            Aggregation aggregation = Aggregation.newAggregation(matchStage, groupStage);

            AggregationResults<TotalAmount> results = mongoTemplate.aggregate(
                    aggregation, "finance", TotalAmount.class);

            return results.getUniqueMappedResult() != null ? results.getUniqueMappedResult().getTotalAmount() : 0.0;
        } catch (Exception e) {
            e.printStackTrace();
            return 0.0;
        }
    }

    // Helper class to map aggregation results
    private static class TotalAmount {
        private Double totalAmount;

        public Double getTotalAmount() {
            return totalAmount;
        }

        public void setTotalAmount(Double totalAmount) {
            this.totalAmount = totalAmount;
        }
    }
}