import { AllergyCatalogItem } from "../domain/allergyCatalogItem";
import { MedicalConditionCatalog } from "../domain/medicalConditionCatalog";
import IMedicalConditionCatalogDTO from "./IMedicalConditionCatalogDTO";

export interface IMedicalConditionDTO {
    name: string;
    date: Date;
}