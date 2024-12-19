import { Mapper } from "../core/infra/Mapper";
import { Container } from 'typedi';

import { Document, Model } from 'mongoose';
import { IMedicalRecordPersistence } from "../dataschema/IMedicalRecordPersistence";
import IMedicalRecordDTO from '../dto/IMedicalRecordDTO';
import { MedicalRecord } from "../domain/medicalRecord";
import { Allergy } from "../domain/allergy";
import { MedicalCondition } from "../domain/medicalCondition";
import { AllergyCatalogItem } from "../domain/allergyCatalogItem";
import { MedicalConditionCatalog } from "../domain/medicalConditionCatalog";
import { AllergyCatalogMap } from "./AllergyCatalogMap";
import IAllergyCathalogItemDTO from "../dto/IAllergyCatalogItemDTO";
import AllergyCatalogRepo from "../repos/allergyCatalogRepo";
import MedicalConditionRepo from "../repos/medicalConditionRepo";

export class MedicalRecordMap extends Mapper<MedicalRecord> {
  
  public static toDTO(medicalRecord: MedicalRecord): IMedicalRecordDTO {
    return {
        id: medicalRecord.id.toString(),  
        patientId: medicalRecord.patientId,
        allergies: medicalRecord.allergies.map(allergy => {

            return {
                id: allergy.id.toString(),  // Certificando-se de que o id está sendo convertido corretamente
                code: allergy.code,
                designation: allergy.designation,
                description: allergy.description
            };
        }),
        medicalConditions: medicalRecord.medicalConditions.map(med => {

            return {
                id: med.id.toString(),  // Certificando-se de que o id está sendo convertido corretamente
                code: med.code,
                designation: med.designation,
                date: med.date 
            };
        })
    } as unknown as IMedicalRecordDTO;
}

public static async toDomain(medicalRecord: any): Promise<MedicalRecord> {
    const repoAllergiesCatalogRepo = Container.get(AllergyCatalogRepo);
    const medicalConditionsCatalogRepo = Container.get(MedicalConditionRepo);
  
    const allergies = await Promise.all(
      medicalRecord.allergies.map(async (allergy: any) => {
        const allergyCatalogItem = await repoAllergiesCatalogRepo.findByCode(allergy.code);
        return Allergy.create(allergyCatalogItem, allergy.description, allergy.id); // Passando o catálogo ao invés do id
      })
    );
  
    const medicalConditions = await Promise.all(
      medicalRecord.medicalConditions.map(async (condition: any) => {
        const medicalConditionCatalog = await medicalConditionsCatalogRepo.findByCode(condition.code);
        return MedicalCondition.create(medicalConditionCatalog, new Date(condition.date), condition.id); // Passando o catálogo e a data
      })
    );
  
    // Extrair os valores dos resultados para impressão
    const extractedAllergies = allergies.map(result => result.getValue());
    const extractedMedicalConditions = medicalConditions.map(result => result.getValue());
    

    const medicalRecordOrError = MedicalRecord.create(
      medicalRecord.patientId, 
      extractedAllergies,
      extractedMedicalConditions,
      medicalRecord.id
    );

  
    if (medicalRecordOrError.isFailure) {
      throw new Error(medicalRecordOrError.error.toString());
    }
  
    return medicalRecordOrError.getValue();
  }
  

  
  public static toPersistence(medicalRecord: MedicalRecord): any {
    return {
        id: medicalRecord.id.toString(),
        patientId: medicalRecord.patientId.toString(),
        allergies: medicalRecord.allergies.map(allergy => ({
            id: allergy.id.toString(),
            code: allergy.code,
            designation: allergy.designation,
            description: allergy.description
        })),
        medicalConditions: medicalRecord.medicalConditions.map(condition => ({
            id: condition.id.toString(),
            code: condition.code,
            designation: condition.designation,
            date: condition.date // Certifique-se de que date seja uma string ou Date válida
        }))
    };
  }

}