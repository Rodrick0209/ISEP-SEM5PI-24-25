import { AllergyCathalogItem } from "../domain/allergyCathalogItem";
import { MedicalCondition } from "../domain/medicalCondition";

export default interface IMedicalRecordDTO {
    patientId: string;
    allergies: AllergyCathalogItem[];
    medicalConditions: MedicalCondition[]
}