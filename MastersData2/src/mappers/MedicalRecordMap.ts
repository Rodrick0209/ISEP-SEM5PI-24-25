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
                name: allergy.name, 
                description: allergy.description
            };
        }),
        medicalConditions: medicalRecord.medicalConditions.map(med => {

            return {
                id: med.id.toString(),  // Certificando-se de que o id está sendo convertido corretamente
                name: med.name,
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
        const allergyCatalogItem = await repoAllergiesCatalogRepo.findByAllergyName(allergy.name);
        return Allergy.create(allergyCatalogItem, allergy.description); // Passando o catálogo ao invés do id
      })
    );
  
    const medicalConditions = await Promise.all(
      medicalRecord.medicalConditions.map(async (condition: any) => {
        const medicalConditionCatalog = await medicalConditionsCatalogRepo.findByMedicalConditionName(condition.name);
        return MedicalCondition.create(medicalConditionCatalog, new Date(condition.date)); // Passando o catálogo e a data
      })
    );
  
    // Extrair os valores dos resultados para impressão
    const extractedAllergies = allergies.map(result => result.getValue());
    const extractedMedicalConditions = medicalConditions.map(result => result.getValue());
    console.log('paTIENT  ID = ',medicalRecord.patientId);

    const medicalRecordOrError = MedicalRecord.create(
      medicalRecord.patientId, 
      extractedAllergies,
      extractedMedicalConditions,
      medicalRecord.id
    );

    console.log(medicalRecordOrError);
  
    if (medicalRecordOrError.isFailure) {
      console.log(medicalRecordOrError.error);
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
            name: allergy.name,
            description: allergy.description
        })),
        medicalConditions: medicalRecord.medicalConditions.map(condition => ({
            id: condition.id.toString(),
            name: condition.name,
            date: condition.date // Certifique-se de que date seja uma string ou Date válida
        }))
    };
  }

}