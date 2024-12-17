import { AllergyCatalogItem } from "../domain/allergyCatalogItem";
import IAllergyCathalogItemDTO from "./IAllergyCatalogItemDTO";

export interface IAllergyDTO {
    id: string;
    code: string;
    designation: string;
    description: string;
}