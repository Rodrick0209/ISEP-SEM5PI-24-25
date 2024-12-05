import { Mapper } from "../core/infra/Mapper";
import { Document, Model } from 'mongoose';
import { IMedicalRecordPersistence } from "../dataschema/IMedicalRecordPersistence";
import IMedicalRecordDTO from '../dto/IMedicalRecordDTO';
import { MedicalRecord } from "../domain/medicalRecord";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Allergy } from "../domain/allergy";

export class MedicalRecordMap extends Mapper<MedicalRecord> {
  
  public static toDTO( medicalRecord: MedicalRecord): IMedicalRecordDTO {
    return {
      id: medicalRecord.id.toString(),
      patientId: medicalRecord.patientId,
      allergies: medicalRecord.allergies.map(allergy => allergy.name),
      medicalConditions: medicalRecord.medicalConditions.map(cond => cond.name)
    } as IMedicalRecordDTO;
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