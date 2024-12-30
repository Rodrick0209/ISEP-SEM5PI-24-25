import { Allergy } from "../../../src/domain/allergy";
import { AllergyCatalogItem } from "../../../src/domain/allergyCatalogItem";
import IAllergyCatalogItemDTO from "../../../src/dto/IAllergyCatalogItemDTO";
import { Result } from "../../../src/core/logic/Result";
import { UniqueEntityID } from "../../../src/core/domain/UniqueEntityID";

// Static GUID to use across tests
const STATIC_GUID = new UniqueEntityID("123e4567-e89b-12d3-a456-426614174000");

describe('Allergy', () => {

    it('should create a valid Allergy with valid DTO', () => {
        const validDTO: IAllergyCatalogItemDTO = {
            id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123456",
            designation: "Peanut Allergy",
            description: "An allergic reaction to peanuts.",
        };

        const result = AllergyCatalogItem.create(validDTO);

        // Check if the result is successful
        if (result.isFailure) {
            throw new Error("Failed to create AllergyCatalogItem");
        }

        const validAllergyCatalogItem = result.getValue();
        const allergyResult = Allergy.create(validAllergyCatalogItem, "description", STATIC_GUID);

        expect(allergyResult.isSuccess).toBe(true);
        expect(allergyResult.getValue()).toBeInstanceOf(Allergy);

        const allergy = allergyResult.getValue();
        expect(allergy.code).toBe(validAllergyCatalogItem.code);
        expect(allergy.designation).toBe(validAllergyCatalogItem.designation);
        expect(allergy.description).toBe("description");
        expect(allergy.id).toBe(STATIC_GUID);
    });

    it('should fail to create Allergy if AllergyCatalogItem or description is null', () => {
        const invalidAllergyCatalogItem = null;
        const description = "An allergic reaction to peanuts.";

        const result = Allergy.create(invalidAllergyCatalogItem, description, STATIC_GUID);

        expect(result.isSuccess).toBe(false);
        expect(result.error).toBe('Must provide a valid allergy and description for the allergy');
    });

    it('should fail to create Allergy if description is null', () => {
        const validDTO: IAllergyCatalogItemDTO = {
            id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123456",
            designation: "Peanut Allergy",
            description: "An allergic reaction to peanuts.",
        };

        const result = AllergyCatalogItem.create(validDTO);
        if (result.isFailure) {
            throw new Error("Failed to create AllergyCatalogItem");
        }

        const validAllergyCatalogItem = result.getValue();
        const description = null;

        const allergyResult = Allergy.create(validAllergyCatalogItem, description, STATIC_GUID);

        expect(allergyResult.isSuccess).toBe(false);
        expect(allergyResult.error).toBe('Must provide a valid allergy and description for the allergy');
    });

    

    it('should create Allergy with a specific UniqueEntityID', () => {
        const validDTO: IAllergyCatalogItemDTO = {
            id: "d8a9b45f-5e2f-4a67-9435-dfe16be4b72f",
            code: "123456",
            designation: "Peanut Allergy",
            description: "An allergic reaction to peanuts.",
        };
        
const result = AllergyCatalogItem.create(validDTO);
        if (result.isFailure) {
            throw new Error("Failed to create AllergyCatalogItem");
        }

        const validAllergyCatalogItem = result.getValue();
        const description = "An allergic reaction to peanuts.";

        const allergyResult = Allergy.create(validAllergyCatalogItem, description, STATIC_GUID);

        expect(allergyResult.isSuccess).toBe(true);
        const allergy = allergyResult.getValue();
        expect(allergy.id).toBe(STATIC_GUID);
    });

});