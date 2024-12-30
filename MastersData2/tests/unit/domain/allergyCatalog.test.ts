import { UniqueEntityID } from "../../../src/core/domain/UniqueEntityID";
import { Result } from "../../../src/core/logic/Result";
import { AllergyCatalogItem } from "../../../src/domain/allergyCatalogItem";
import IAllergyCatalogItemDTO from "../../../src/dto/IAllergyCatalogItemDTO";

describe('AllergyCatalogItem', () => {

    it('should create a valid AllergyCatalogItem with valid DTO', () => {
        const validDTO: IAllergyCatalogItemDTO = {
            id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123456",
            designation: "Peanut Allergy",
            description: "An allergic reaction to peanuts.",
        };

        const result = AllergyCatalogItem.create(validDTO);

        expect(result.isSuccess).toBe(true);
        expect(result.getValue()).toBeInstanceOf(AllergyCatalogItem);

        const allergyCatalogItem = result.getValue();
        expect(allergyCatalogItem.code).toBe(validDTO.code);
        expect(allergyCatalogItem.designation).toBe(validDTO.designation);
        expect(allergyCatalogItem.description).toBe(validDTO.description);
    });

    it('should set description to null if not provided', () => {
        const validDTO: IAllergyCatalogItemDTO = {
            id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123456",
            designation: "Peanut Allergy"
        };

        const result = AllergyCatalogItem.create(validDTO);

        expect(result.isSuccess).toBe(true);
        const allergyCatalogItem = result.getValue();
        expect(allergyCatalogItem.description).toBeNull();
    });

    it('should allow updating designation', () => {
        const validDTO: IAllergyCatalogItemDTO = {
            id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123456",
            designation: "Initial Allergy",
            description: "Initial description"
        };

        const result = AllergyCatalogItem.create(validDTO);

        expect(result.isSuccess).toBe(true);
        const allergyCatalogItem = result.getValue();

        allergyCatalogItem.designation = "Updated Allergy";
        expect(allergyCatalogItem.designation).toBe("Updated Allergy");
    });

    it('should allow updating description', () => {
        const validDTO: IAllergyCatalogItemDTO = {
            id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123456",
            designation: "Peanut Allergy",
            description: "Initial description"
        };

        const result = AllergyCatalogItem.create(validDTO);

        expect(result.isSuccess).toBe(true);
        const allergyCatalogItem = result.getValue();

        allergyCatalogItem.description = "Updated description";
        expect(allergyCatalogItem.description).toBe("Updated description");
    });

    it('should validate valid SNOMED CT code', () => {
        const validDTO: IAllergyCatalogItemDTO = {
            id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123456", // SNOMED CT code
            designation: "Peanut Allergy"
        };

        const result = AllergyCatalogItem.create(validDTO);

        expect(result.isSuccess).toBe(true);
    });

    it('should validate valid ICD-11 code', () => {
        const validDTO: IAllergyCatalogItemDTO = {
            id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "A12.34", // ICD-11 code
            designation: "Peanut Allergy"
        };

        const result = AllergyCatalogItem.create(validDTO);

        expect(result.isSuccess).toBe(true);
    });

    it('should create AllergyCatalogItem with a specific UniqueEntityID', () => {
        const validDTO: IAllergyCatalogItemDTO = {
            id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123456",
            designation: "Peanut Allergy",
            description: "An allergic reaction to peanuts."
        };
        const customId = new UniqueEntityID("custom-id-123");

        const result = AllergyCatalogItem.create(validDTO, customId);

        expect(result.isSuccess).toBe(true);
        const allergyCatalogItem = result.getValue();
        expect(allergyCatalogItem.id).toBe(customId);
    });

});