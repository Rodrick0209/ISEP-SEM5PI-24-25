import { UniqueEntityID } from "../../../src/core/domain/UniqueEntityID";
import { Result } from "../../../src/core/logic/Result";
import { MedicalConditionCatalog } from "../../../src/domain/medicalConditionCatalog";
import IMedicalConditionCatalogDTO from "../../../src/dto/IMedicalConditionCatalogDTO";

describe('MedicalConditionCatalog', () => {

    it('should create a valid MedicalConditionCatalog with valid DTO', () => {
        const validDTO: IMedicalConditionCatalogDTO = {
          id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123456", // SNOMED CT code
            designation: "Peanut Allergy",
            description: "An allergic reaction to peanuts.",
            commonSymptoms: ["Hives", "Itching"]
        };

        const result = MedicalConditionCatalog.create(validDTO);

        expect(result.isSuccess).toBe(true);
        expect(result.getValue()).toBeInstanceOf(MedicalConditionCatalog);

        const medicalConditionCatalog = result.getValue();
        expect(medicalConditionCatalog.code).toBe(validDTO.code);
        expect(medicalConditionCatalog.designation).toBe(validDTO.designation);
        expect(medicalConditionCatalog.description).toBe(validDTO.description);
        expect(medicalConditionCatalog.commonSymptoms).toEqual(validDTO.commonSymptoms);
    });

    it('should set description to null if not provided', () => {
        const validDTO: IMedicalConditionCatalogDTO = {
          id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123456", // SNOMED CT code
            designation: "Peanut Allergy"
        };

        const result = MedicalConditionCatalog.create(validDTO);

        expect(result.isSuccess).toBe(true);
        const medicalConditionCatalog = result.getValue();
        expect(medicalConditionCatalog.description).toBeNull();
    });

    it('should fail to create MedicalConditionCatalog with description exceeding 2048 characters', () => {
        const invalidDTO: IMedicalConditionCatalogDTO = {
          id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123456", // SNOMED CT code
            designation: "Peanut Allergy",
            description: "A".repeat(2049), // Description longer than 2048 characters
        };

        const result = MedicalConditionCatalog.create(invalidDTO);

        expect(result.isSuccess).toBe(false);
        expect(result.error).toBe('Description cannot exceed 2048 characters');
    });

    it('should validate valid SNOMED CT code', () => {
        const validDTO: IMedicalConditionCatalogDTO = {
          id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123456", // SNOMED CT code
            designation: "Peanut Allergy"
        };

        const result = MedicalConditionCatalog.create(validDTO);

        expect(result.isSuccess).toBe(true);
    });

    it('should validate valid ICD-11 code', () => {
        const validDTO: IMedicalConditionCatalogDTO = {
          id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "A12.34", // ICD-11 code
            designation: "Peanut Allergy"
        };

        const result = MedicalConditionCatalog.create(validDTO);

        expect(result.isSuccess).toBe(true);
    });

    it('should fail to create MedicalConditionCatalog with invalid code', () => {
        const invalidDTO: IMedicalConditionCatalogDTO = {
          id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "INVALID_CODE", // Invalid code
            designation: "Peanut Allergy"
        };

        const result = MedicalConditionCatalog.create(invalidDTO);

        expect(result.isSuccess).toBe(false);
        expect(result.error).toBe('Invalid code for the medical condition');
    });

    it('should create MedicalConditionCatalog with a specific UniqueEntityID', () => {
        const validDTO: IMedicalConditionCatalogDTO = {
          id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123456", // SNOMED CT code
            designation: "Peanut Allergy",
            description: "An allergic reaction to peanuts."
        };
        const customId = new UniqueEntityID("custom-id-123");

        const result = MedicalConditionCatalog.create(validDTO, customId);

        expect(result.isSuccess).toBe(true);
        const medicalConditionCatalog = result.getValue();
        expect(medicalConditionCatalog.id).toBe(customId);
    });

});