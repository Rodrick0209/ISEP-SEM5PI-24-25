import { Allergy } from "../domain/allergy";
import { MedicalCondition } from "../domain/medicalCondition";

export default interface IMedicalRecordDTO {
    patientId: string;
    allergies: String[];
    medicalConditions: String[]
}