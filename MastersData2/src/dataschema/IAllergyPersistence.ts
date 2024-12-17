import { AllergyCatalogItem } from "../domain/allergyCatalogItem";

export interface IAllergyPersistence {
  _id: string;
  allergyCatalogItem: AllergyCatalogItem;
  description: string;
}