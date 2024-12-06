import { Service, Inject} from 'typedi';

import { MedicalCondition } from '../domain/medicalCondition';
import { MedicalConditionId } from '../domain/medicalConditionId';
import { MedicalConditionMap } from '../mappers/MedicalConditionMap';

import { Document, FilterQuery, Model } from 'mongoose';
import { IMedicalConditionPersistence } from '../dataschema/IMedicalConditionPersistence';
import IMedicalConditionRepo from '../services/IRepos/IMedicalConditionRepo';

@Service()
export default class MedicalConditionRepo implements IMedicalConditionRepo {
    private models: any;

    constructor(
        @Inject('medicalConditionSchema') private medicalConditionSchema : Model<IMedicalConditionPersistence & Document>,
    ) {}

    public async findByMedicalConditionName(medicalConditionName: string): Promise<MedicalCondition> {
        const query = { name: medicalConditionName };
        const medicalConditionDocument = await this.medicalConditionSchema.findOne(query as FilterQuery<IMedicalConditionPersistence & Document>);
        if (medicalConditionDocument != null) {
            return MedicalConditionMap.toDomain(medicalConditionDocument);
        } else {
            return null;
        }
    }

    private createBaseQuery (): any {
        return {
            where: {},
        }
    }

    public async exists(medicalCondition: MedicalCondition): Promise<boolean> {
        const idX = medicalCondition.id instanceof MedicalConditionId ? (<MedicalConditionId>medicalCondition.id).toValue() : medicalCondition.id;

        const query = { domainId: idX}; 
        const medicalConditionDocument = await this.medicalConditionSchema.findOne( query as FilterQuery<IMedicalConditionPersistence & Document>);

        return !!medicalConditionDocument === true;
    }

    public async save (medicalCondition: MedicalCondition): Promise<MedicalCondition> {
        const query = { domainId: medicalCondition.id.toString()}; 

        const medicalConditionDocument = await this.medicalConditionSchema.findOne( query );

        try {
            if (medicalConditionDocument === null ) {
                const rawMedicalCondition: any = MedicalConditionMap.toPersistence(medicalCondition);

                const medicalConditionCreated = await this.medicalConditionSchema.create(rawMedicalCondition);

                return MedicalConditionMap.toDomain(medicalConditionCreated);
            } else {
                medicalConditionDocument.name = medicalCondition.name;
                await medicalConditionDocument.save();

                return medicalCondition;
            }
        } catch (err) {
            throw err;
        }
    }

    public async findByDomainId (medicalConditionId: MedicalConditionId | string | MedicalCondition): Promise<MedicalCondition> {
        const query = { domainId: medicalConditionId};
        const medicalConditionRecord = await this.medicalConditionSchema.findOne( query as FilterQuery<IMedicalConditionPersistence & Document> );
        if (medicalConditionRecord != null)
            return MedicalConditionMap.toDomain(medicalConditionRecord);
        else
            return null;
    }

    public async findByName(medicalConditionName: string): Promise<MedicalCondition[]> {
        const query = { name: medicalConditionName };
        const medicalConditionRecord = await this.medicalConditionSchema.find(query as FilterQuery<IMedicalConditionPersistence & Document>);
        return medicalConditionRecord.map((medicalConditionRecord) => MedicalConditionMap.toDomain(medicalConditionRecord));
    }

    public async findAll(): Promise<MedicalCondition[]> {
        const medicalConditionRecords = await this.medicalConditionSchema.find();
        return medicalConditionRecords.map((medicalConditionRecord) => MedicalConditionMap.toDomain(medicalConditionRecord));
    }

}