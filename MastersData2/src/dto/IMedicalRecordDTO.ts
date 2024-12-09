import { Allergy } from "../domain/allergy";
import { MedicalCondition } from "../domain/medicalCondition";
import { IAllergyDTO } from "./IAllergyDTO";
import { IMedicalConditionDTO } from "./IMedicalConditionDTO";


export default interface IMedicalRecordDTO {
    id: string;
    patientId: string;
    allergies: IAllergyDTO[];
    medicalConditions: IMedicalConditionDTO[]
}