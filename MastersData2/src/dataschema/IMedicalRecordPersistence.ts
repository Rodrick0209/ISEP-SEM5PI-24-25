import { Allergy } from '../domain/allergy';
import { MedicalCondition } from '../domain/medicalCondition';

export interface IMedicalRecordPersistence {
  _id: string;
  patientId: string;
  allergies: Allergy[];
  medicalConditions: MedicalCondition[];
}