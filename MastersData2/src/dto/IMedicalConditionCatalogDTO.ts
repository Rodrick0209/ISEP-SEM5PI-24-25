export default interface IMedicalConditionCatalogDTO {
    id: string;
    code: string;
    designation: string;
    description?: string;
    commonSymptoms?: string[];
}