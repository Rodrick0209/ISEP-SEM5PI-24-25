import { MedicalConditionCatalog } from "../domain/medicalConditionCatalog";

export interface IMedicalConditionPersistence {
    _id: string;
    medicalConditionCatalog: MedicalConditionCatalog;
    date: Date
}