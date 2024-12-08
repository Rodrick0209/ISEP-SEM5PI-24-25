import { Service, Inject} from 'typedi';

import { MedicalConditionCatalog } from '../domain/medicalConditionCatalog';
import { MedicalConditionCatalogId } from '../domain/medicalConditionCatalogId';
import { MedicalConditionCatalogMap } from '../mappers/MedicalConditionCatalogMap';

import { Document, FilterQuery, Model } from 'mongoose';
import { IMedicalConditionPersistence } from '../dataschema/IMedicalConditionPersistence';
import IMedicalConditionRepo from '../services/IRepos/IMedicalConditionRepo';

@Service()
export default class MedicalConditionRepo implements IMedicalConditionRepo {
    private models: any;

    constructor(
        @Inject('medicalConditionSchema') private medicalConditionSchema : Model<IMedicalConditionPersistence & Document>,
    ) {}

    public async findByMedicalConditionName(medicalConditionName: string): Promise<MedicalConditionCatalog> {
        const query = { name: medicalConditionName };
        
        const medicalConditionDocument = await this.medicalConditionSchema.findOne(query as FilterQuery<IMedicalConditionPersistence & Document>);

        if (medicalConditionDocument != null) {
            return MedicalConditionCatalogMap.toDomain(medicalConditionDocument);
        } else {
            return null;
        }
    }

    private createBaseQuery (): any {
        return {
            where: {},
        }
    }

    public async exists(medicalCondition: MedicalConditionCatalog): Promise<boolean> {
        const idX = medicalCondition.id instanceof MedicalConditionCatalogId ? (<MedicalConditionCatalogId>medicalCondition.id).toValue() : medicalCondition.id;

        const query = { domainId: idX}; 
        const medicalConditionDocument = await this.medicalConditionSchema.findOne( query as FilterQuery<IMedicalConditionPersistence & Document>);

        return !!medicalConditionDocument === true;
    }

    public async save (medicalCondition: MedicalConditionCatalog): Promise<MedicalConditionCatalog> {
        const query = { domainId: medicalCondition.id.toString()}; 

        const medicalConditionDocument = await this.medicalConditionSchema.findOne( query );

        try {
            if (medicalConditionDocument === null ) {
                const rawMedicalCondition: any = MedicalConditionCatalogMap.toPersistence(medicalCondition);

                const medicalConditionCreated = await this.medicalConditionSchema.create(rawMedicalCondition);

                return MedicalConditionCatalogMap.toDomain(medicalConditionCreated);
            } else {
                medicalConditionDocument.name = medicalCondition.name;
                await medicalConditionDocument.save();

                return medicalCondition;
            }
        } catch (err) {
            throw err;
        }
    }

    public async findByDomainId (medicalConditionId:  string | MedicalConditionCatalog): Promise<MedicalConditionCatalog> {
        const query = { domainId: medicalConditionId };
        const medicalConditionRecord = await this.medicalConditionSchema.findOne(query as FilterQuery<IMedicalConditionPersistence & Document>);
        if (medicalConditionRecord != null) {
            return MedicalConditionCatalogMap.toDomain(medicalConditionRecord);
        } else {
            return null;
        }
    }

    public async findByName(medicalConditionName: string): Promise<MedicalConditionCatalog[]> {
        const query = { name: medicalConditionName };
        const medicalConditionRecord = await this.medicalConditionSchema.find(query as FilterQuery<IMedicalConditionPersistence & Document>);
        return medicalConditionRecord.map((medicalConditionRecord) => MedicalConditionCatalogMap.toDomain(medicalConditionRecord));
    }

    public async findAll(): Promise<MedicalConditionCatalog[]> {
        const medicalConditionRecords = await this.medicalConditionSchema.find();
        return medicalConditionRecords.map((medicalConditionRecord) => MedicalConditionCatalogMap.toDomain(medicalConditionRecord));
    }

}