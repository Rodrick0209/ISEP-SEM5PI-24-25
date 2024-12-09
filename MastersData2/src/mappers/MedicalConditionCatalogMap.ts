import { Mapper } from "../core/infra/Mapper";
import { Document, Model } from 'mongoose';
import { IMedicalConditionPersistence } from "../dataschema/IMedicalConditionPersistence";
import IMedicalConditionDTO from '../dto/IMedicalConditionCatalogDTO';
import { MedicalConditionCatalog } from "../domain/medicalConditionCatalog";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { MedicalCondition } from "../domain/medicalCondition";

export class MedicalConditionCatalogMap extends Mapper<MedicalConditionCatalog> {

  public static toDTO(medicalCondition: MedicalConditionCatalog): IMedicalConditionDTO {
    return {
      id: medicalCondition.id.toString(),
      name: medicalCondition.name,
    } as IMedicalConditionDTO;
  }

  public static toDomain(medicalCondition: any | Model<IMedicalConditionPersistence & Document>): MedicalConditionCatalog {
    const medicalConditionOrError = MedicalConditionCatalog.create(
      medicalCondition,
      medicalCondition.domainId
    );
    medicalConditionOrError.isFailure ? console.log(medicalConditionOrError.error) : '';

    return medicalConditionOrError.isSuccess ? medicalConditionOrError.getValue() : null;
  }

  public static toPersistence(medicalCondition: MedicalConditionCatalog): any {
    return {
      domainId: medicalCondition.id.toString(),
      name: medicalCondition.name
    }
  }
}