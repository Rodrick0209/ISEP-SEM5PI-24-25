import { Service, Inject } from 'typedi';

import { Document, FilterQuery, Model } from 'mongoose';

import { IAllergyCatalogItemPersistence } from '../dataschema/IAllergyCatalogItemPersistence';

import IAllergyCatalogRepo from '../services/IRepos/IAllergyCatalogRepo';
import { AllergyCathalogItem } from '../domain/allergyCathalogItem';
import { AllergyCathalogItemId } from '../domain/allergyCathalogId';
import { AllergyMap } from '../mappers/AllergyMap';

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

    public async exists(allergy: AllergyCathalogItem): Promise<boolean> {
        const idX = allergy.id instanceof AllergyCathalogItemId ? (<AllergyCathalogItemId>allergy.id).toValue() : allergy.id;

        const query = { domainId: idX };

        const allergyDocument = await this.allergySchema.findOne(query as FilterQuery<IAllergyCatalogItemPersistence & Document>);

        return !!allergyDocument === true;
    }

    public async save (allergy: AllergyCathalogItem): Promise<AllergyCathalogItem> {
        const query = { domainId: allergy.id.toString()};

        const allergyDocument = await this.allergySchema.findOne( query );
        try {
            if (allergyDocument === null) {
                const rawAllergy: any = AllergyMap.toPersistence(allergy);
                const allergyCreated = await this.allergySchema.create(rawAllergy);

                return AllergyMap.toDomain(allergyCreated);
            } else {
                allergyDocument.name = allergy.name;
                await allergyDocument.save();

                return allergy;
            }
        }  catch (err) {
            throw err;
        }
    }


    public async findByAllergyName(name: string): Promise<AllergyCathalogItem> {
        const query = { name: name };
        const allergyDocument = await this.allergySchema.findOne(query as FilterQuery<IAllergyCatalogItemPersistence & Document>);
        if (allergyDocument != null) {
            return AllergyMap.toDomain(allergyDocument);
        } else {
            return null;
        }
    }

    public async findAll(): Promise<AllergyCathalogItem[]> {
        const allergyDocuments = await this.allergySchema.find();
        return allergyDocuments.map(doc => AllergyMap.toDomain(doc));
    }


    public async findById(allergyId: AllergyCathalogItemId | string): Promise<AllergyCathalogItem> {
        const idX = allergyId instanceof AllergyCathalogItemId ? (<AllergyCathalogItemId>allergyId).toValue() : allergyId;
        const query = { domainId: idX };
        const allergyDocument = await this.allergySchema.findOne(query as FilterQuery<IAllergyCatalogItemPersistence & Document>);
        if (allergyDocument != null) {
            return AllergyMap.toDomain(allergyDocument);
        } else {
            return null;
        }
    }






}

