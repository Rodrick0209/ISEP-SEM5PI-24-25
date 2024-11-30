import { Service, Inject } from 'typedi';

import { Document, FilterQuery, Model } from 'mongoose';

import { IAllergyCatalogItemPersistence } from '../dataschema/IAllergyCatalogItemPersistence';

import IAllergyCatalogRepo from '../services/IRepos/IAllergyCatalogRepo';
import { AllergyCatalogItem } from '../domain/allergyCatalogItem';
import { AllergyCatalogItemId } from '../domain/allergyCatalogId';
import { AllergyCatalogMap } from '../mappers/AllergyCatalogMap';

@Service()
export default class AllergyCatalogRepo implements IAllergyCatalogRepo {
    private models: any;

    constructor(
        @Inject('allergyCatalogSchema') private allergySchema: Model<IAllergyCatalogItemPersistence & Document>,
    ) {}

    private createBaseQuery(): any {
        return {
            where: {},
        };
    }

    public async exists(allergy: AllergyCatalogItem): Promise<boolean> {
        const idX = allergy.id instanceof AllergyCatalogItemId ? (<AllergyCatalogItemId>allergy.id).toValue() : allergy.id;

        const query = { domainId: idX };

        const allergyDocument = await this.allergySchema.findOne(query as FilterQuery<IAllergyCatalogItemPersistence & Document>);

        return !!allergyDocument === true;
    }

    public async save (allergy: AllergyCatalogItem): Promise<AllergyCatalogItem> {
        const query = { domainId: allergy.id.toString()};

        const allergyDocument = await this.allergySchema.findOne( query );
        try {
            if (allergyDocument === null) {
                const rawAllergy: any = AllergyCatalogMap.toPersistence(allergy);
                const allergyCreated = await this.allergySchema.create(rawAllergy);

                return AllergyCatalogMap.toDomain(allergyCreated);
            } else {
                allergyDocument.name = allergy.name;
                await allergyDocument.save();

                return allergy;
            }
        }  catch (err) {
            throw err;
        }
    }


    public async findByAllergyName(name: string): Promise<AllergyCatalogItem> {
        const query = { name: name };
        const allergyDocument = await this.allergySchema.findOne(query as FilterQuery<IAllergyCatalogItemPersistence & Document>);
        if (allergyDocument != null) {
            return AllergyCatalogMap.toDomain(allergyDocument);
        } else {
            return null;
        }
    }

    public async findAll(): Promise<AllergyCatalogItem[]> {
        const allergyDocuments = await this.allergySchema.find();
        return allergyDocuments.map(doc => AllergyCatalogMap.toDomain(doc));
    }


    public async findById(allergyId: AllergyCatalogItemId | string): Promise<AllergyCatalogItem> {
        const idX = allergyId instanceof AllergyCatalogItemId ? (<AllergyCatalogItemId>allergyId).toValue() : allergyId;
        const query = { domainId: idX };
        const allergyDocument = await this.allergySchema.findOne(query as FilterQuery<IAllergyCatalogItemPersistence & Document>);
        if (allergyDocument != null) {
            return AllergyCatalogMap.toDomain(allergyDocument);
        } else {
            return null;
        }
    }






}

