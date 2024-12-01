import { Mapper } from "../core/infra/Mapper";
import { Document, Model } from 'mongoose';
import { IMedicalConditionPersistence } from "../dataschema/IMedicalConditionPersistence";
import IMedicalConditionDTO from '../dto/IMedicalConditionDTO';
import { MedicalCondition } from "../domain/medicalCondition";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";

export class MedicalConditionMap extends Mapper<MedicalCondition> {
  
  public static toDTO( medicalCondition: MedicalCondition): IMedicalConditionDTO {
    return {
      id: medicalCondition.id.toString(),
      name: medicalCondition.name,
    } as IMedicalConditionDTO;
  }

  public static toDomain (medicalCondition: any | Model<IMedicalConditionPersistence & Document> ): MedicalCondition {
    const medicalConditionOrError = MedicalCondition.create(
      medicalCondition,
      new UniqueEntityID(medicalCondition.domainId)
    );

    medicalConditionOrError.isFailure ? console.log(medicalConditionOrError.error) : '';

    return medicalConditionOrError.isSuccess ? medicalConditionOrError.getValue() : null;
  }

  public static toPersistence (medicalCondition: MedicalCondition): any {
    return {
      domainId: medicalCondition.id.toString(),
      name: medicalCondition.name
    }
  }
}