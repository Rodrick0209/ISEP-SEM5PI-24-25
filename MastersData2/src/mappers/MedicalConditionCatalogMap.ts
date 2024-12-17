import { Mapper } from "../core/infra/Mapper";
import { Document, Model } from 'mongoose';
import { IMedicalConditionPersistence } from "../dataschema/IMedicalConditionPersistence";
import IMedicalConditionDTO from '../dto/IMedicalConditionCatalogDTO';
import { MedicalConditionCatalog } from "../domain/medicalConditionCatalog";
import IMedicalConditionCatalogDTO from "../dto/IMedicalConditionCatalogDTO";
import { IMedicalConditionCatalogPersistence } from "../dataschema/IMedicalConditionCatalogPersistence";


export class MedicalConditionCatalogMap extends Mapper<MedicalConditionCatalog> {

  public static toDTO(medicalCondition: MedicalConditionCatalog): IMedicalConditionCatalogDTO {
    return {
      id: medicalCondition.id.toString(),
      code: medicalCondition.code,
      designation: medicalCondition.designation,
      description: medicalCondition.description ?? null,
      commonSymptoms: medicalCondition.commonSymptoms ?? null
    } as IMedicalConditionCatalogDTO
  }

  public static toDomain(medicalCondition: any | Model<IMedicalConditionCatalogPersistence & Document>): MedicalConditionCatalog {
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
      code: medicalCondition.code,
      designation: medicalCondition.designation,
      description: medicalCondition.description ?? null,
      commonSymptoms: medicalCondition.commonSymptoms
    }
  }
}