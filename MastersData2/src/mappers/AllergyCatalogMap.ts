import { Container } from 'typedi';

import { Mapper } from "../core/infra/Mapper";
import { Document, Model } from 'mongoose';

import { UniqueEntityID } from '../core/domain/UniqueEntityID';

import { AllergyCatalogItem } from "../domain/allergyCatalogItem";

import IAllergyCathalogItemDTO from '../dto/IAllergyCatalogItemDTO';
import { IAllergyCatalogItemPersistence } from '../dataschema/IAllergyCatalogItemPersistence';

export class AllergyCatalogMap extends Mapper<AllergyCatalogItem> {

    public static toDTO (allergy: AllergyCatalogItem): IAllergyCathalogItemDTO {
        return {
            id: allergy.id.toString(),
            name: allergy.name
        } as IAllergyCathalogItemDTO;
    }

    public static toDomain (allergy: any | Model<IAllergyCatalogItemPersistence & Document>): AllergyCatalogItem {
        const allergyOrError = AllergyCatalogItem.create(
            allergy,
            new UniqueEntityID(allergy.domainId)
        );

        allergyOrError.isFailure ? console.log(allergyOrError.error) : '';

        return allergyOrError.isSuccess ? allergyOrError.getValue() : null;
    }

    public static toPersistence (allergy: AllergyCatalogItem): any {
        return {
            domainId: allergy.id.toString(),
            name: allergy.name
        };
    }


    
}
