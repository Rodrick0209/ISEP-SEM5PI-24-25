import { MedicalCondition } from "../../../src/domain/medicalCondition";
import { MedicalConditionCatalog } from "../../../src/domain/medicalConditionCatalog";
import { IMedicalConditionDTO } from "../../../src/dto/IMedicalConditionDTO";
import { Result } from "../../../src/core/logic/Result";
import { UniqueEntityID } from "../../../src/core/domain/UniqueEntityID";

// Static GUID to use across tests
const STATIC_GUID = new UniqueEntityID("123e4567-e89b-12d3-a456-426614174000");

describe('MedicalCondition', () => {

    it('should create a valid MedicalCondition with valid DTO and date', () => {
        const validDTO: IMedicalConditionDTO = {
            id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123456",
            designation: "Hypertension",
            date: new Date("2025-12-12")
            // No description field required anymore
        };

        const result = MedicalConditionCatalog.create(validDTO);

        // Check if the result is successful
        if (result.isFailure) {
            throw new Error("Failed to create MedicalConditionCatalog");
        }

        const validMedicalConditionCatalog = result.getValue();
        const date = new Date();  // Use the current date

        const medicalConditionResult = MedicalCondition.create(validMedicalConditionCatalog, date, STATIC_GUID);

        expect(medicalConditionResult.isSuccess).toBe(true);
        expect(medicalConditionResult.getValue()).toBeInstanceOf(MedicalCondition);

        const medicalCondition = medicalConditionResult.getValue();
        expect(medicalCondition.code).toBe(validMedicalConditionCatalog.code);
        expect(medicalCondition.designation).toBe(validMedicalConditionCatalog.designation);
        expect(medicalCondition.date).toBe(date);
        expect(medicalCondition.id).toBe(STATIC_GUID);
    });

    it('should fail to create MedicalCondition if MedicalConditionCatalog or date is null', () => {
        const invalidMedicalConditionCatalog = null;
        const date = new Date();

        const result = MedicalCondition.create(invalidMedicalConditionCatalog, date, STATIC_GUID);

        expect(result.isSuccess).toBe(false);
        expect(result.error).toBe('Must provide a valid medical condition and date for the medical condition');
    });

    it('should fail to create MedicalCondition if date is null', () => {
        const validDTO: IMedicalConditionDTO = {
            id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123456",
            designation: "Hypertension",
            date: new Date("2025-12-12")
            // No description field required anymore
        };

        const result = MedicalConditionCatalog.create(validDTO);
        if (result.isFailure) {
            throw new Error("Failed to create MedicalConditionCatalog");
        }

        const validMedicalConditionCatalog = result.getValue();
        const date = null;

        const medicalConditionResult = MedicalCondition.create(validMedicalConditionCatalog, date, STATIC_GUID);

        expect(medicalConditionResult.isSuccess).toBe(false);
        expect(medicalConditionResult.error).toBe('Must provide a valid medical condition and date for the medical condition');
    });

    it('should create MedicalCondition with a specific UniqueEntityID', () => {
        const validDTO: IMedicalConditionDTO = {
            id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123456",
            designation: "Hypertension",
            date: new Date("2025-12-12")
            // No description field required anymore
        };

        const result = MedicalConditionCatalog.create(validDTO);
        if (result.isFailure) {
            throw new Error("Failed to create MedicalConditionCatalog");
        }

        const validMedicalConditionCatalog = result.getValue();
        const date = new Date();

        const medicalConditionResult = MedicalCondition.create(validMedicalConditionCatalog, date, STATIC_GUID);

        expect(medicalConditionResult.isSuccess).toBe(true);
        const medicalCondition = medicalConditionResult.getValue();
        expect(medicalCondition.id).toBe(STATIC_GUID);
    });

});