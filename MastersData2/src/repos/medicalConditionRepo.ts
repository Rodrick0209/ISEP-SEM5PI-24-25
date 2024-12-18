import { Service, Inject} from 'typedi';

import { MedicalConditionCatalog } from '../domain/medicalConditionCatalog';
import { MedicalConditionCatalogId } from '../domain/medicalConditionCatalogId';
import { MedicalConditionCatalogMap } from '../mappers/MedicalConditionCatalogMap';

import { Document, FilterQuery, Model } from 'mongoose';
import { IMedicalConditionPersistence } from '../dataschema/IMedicalConditionPersistence';
import IMedicalConditionRepo from '../services/IRepos/IMedicalConditionRepo';
import { IMedicalConditionCatalogPersistence } from '../dataschema/IMedicalConditionCatalogPersistence';
import { MedicalCondition } from '../domain/medicalCondition';

@Service()
export default class MedicalConditionRepo implements IMedicalConditionRepo {
    private models: any;

    constructor(
        @Inject('medicalConditionCatalogSchema') private medicalConditionSchema : Model<IMedicalConditionCatalogPersistence & Document>,
    ) {}

    public async findByCode(code: string): Promise<MedicalConditionCatalog> {
        const query = { code: code };
        
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

        const medicalConditionDocument = await this.medicalConditionSchema.findOne( query as FilterQuery<IMedicalConditionCatalogPersistence & Document>);

        try {
            if (medicalConditionDocument === null ) {
                const rawMedicalCondition: any = MedicalConditionCatalogMap.toPersistence(medicalCondition);

                const medicalConditionCreated = await this.medicalConditionSchema.create(rawMedicalCondition);

                return MedicalConditionCatalogMap.toDomain(medicalConditionCreated);
            } else {
                medicalConditionDocument.code = medicalCondition.code;
                medicalConditionDocument.designation = medicalCondition.designation;
                medicalConditionDocument.description = medicalCondition.description;
                medicalConditionDocument.commonSymptoms = medicalCondition.commonSymptoms;
                await medicalConditionDocument.save();

                return medicalCondition;
            }
        } catch (err) {
            throw err;
        }
    }

    public async findByDomainId(medicalConditionId: MedicalConditionCatalogId | string | MedicalCondition): Promise<MedicalConditionCatalog> {
        const query = { domainId: medicalConditionId };
        const medicalConditionRecord = await this.medicalConditionSchema.findOne(query as FilterQuery<IMedicalConditionPersistence & Document>);
        if (medicalConditionRecord != null) {
            return MedicalConditionCatalogMap.toDomain(medicalConditionRecord);
        } else {
            return null;
        }
    }

    public async findAll(): Promise<MedicalConditionCatalog[]> {
        const medicalConditionRecords = await this.medicalConditionSchema.find();
        return medicalConditionRecords.map((medicalConditionRecord) => MedicalConditionCatalogMap.toDomain(medicalConditionRecord));
    }

}