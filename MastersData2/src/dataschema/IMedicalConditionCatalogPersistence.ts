export interface IMedicalConditionCatalogPersistence {
    _id: string;
    code: string;
    designation: string;
    description?: string;
    commonSymptoms?: string[];
}