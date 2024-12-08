import { Mapper } from "../core/infra/Mapper";
import { Document, Model } from 'mongoose';
import { IMedicalRecordPersistence } from "../dataschema/IMedicalRecordPersistence";
import IMedicalRecordDTO from '../dto/IMedicalRecordDTO';
import { MedicalRecord } from "../domain/medicalRecord";


export class MedicalRecordMap extends Mapper<MedicalRecord> {
  
  public static toDTO( medicalRecord: MedicalRecord): IMedicalRecordDTO {
    return {
      patientId: medicalRecord.patientId,
      allergies: medicalRecord.allergies.map(allergy => ({
          name: allergy.name, // Extraindo o nome do item do catálogo
          description: allergy.description
      })),
      medicalConditions: medicalRecord.medicalConditions.map(med => ({
          name: med.name, // Extraindo o nome do item do catálogo
          date: med.date.toISOString().split('T')[0] // Formatando a data para "YYYY-MM-DD"
      }))
  } as unknown as IMedicalRecordDTO;
  
  }

  public static toDomain (medicalRecord: any | Model<IMedicalRecordPersistence & Document> ): MedicalRecord {
    const medicalConditionOrError = MedicalRecord.create(
      medicalRecord,
      medicalRecord.allergies,
      medicalRecord.medicalConditions,
      medicalRecord.id
    );

    medicalConditionOrError.isFailure ? console.log(medicalConditionOrError.error) : '';

    return medicalConditionOrError.isSuccess ? medicalConditionOrError.getValue() : null;
  }

  public static toPersistence (medicalCondition: MedicalRecord): any {
    return {
      id: medicalCondition.id.toString(),
      patientId: medicalCondition.patientId,
      allergies: medicalCondition.allergies.map(allergy => allergy.id),  
      medicalConditions: medicalCondition.medicalConditions.map(cond => cond.id)
    }
  }
}