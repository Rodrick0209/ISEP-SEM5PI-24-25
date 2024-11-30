import { Container } from 'typedi';

import { Mapper } from "../core/infra/Mapper";
import { Document, Model } from 'mongoose';

import { UniqueEntityID } from '../core/domain/UniqueEntityID';

import { AllergyCathalogItem } from "../domain/allergyCathalogItem";

import IAllergyCathalogItemDTO from '../dto/IAllergyCatalogItemDTO';
import { IAllergyCatalogItemPersistence } from '../dataschema/IAllergyCatalogItemPersistence';

export class AllergyMap extends Mapper<AllergyCathalogItem> {

    public static toDTO (allergy: AllergyCathalogItem): IAllergyCathalogItemDTO {
        return {
            id: allergy.id.toString(),
            name: allergy.name
        } as IAllergyCathalogItemDTO;
    }

    public static toDomain (allergy: any | Model<IAllergyCatalogItemPersistence & Document>): AllergyCathalogItem {
        const allergyOrError = AllergyCathalogItem.create(
            allergy,
            new UniqueEntityID(allergy.domainId)
        );

        allergyOrError.isFailure ? console.log(allergyOrError.error) : '';

        return allergyOrError.isSuccess ? allergyOrError.getValue() : null;
    }

    public static toPersistence (allergy: AllergyCathalogItem): any {
        return {
            domainId: allergy.id.toString(),
            name: allergy.name
        };
    }


    
}
