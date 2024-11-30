import { AllergyCatalogItem } from "../domain/allergyCatalogItem";

export interface IAllergyDTO {
    id: string;
    allergyCatalogItem: AllergyCatalogItem;
    date: Date
}