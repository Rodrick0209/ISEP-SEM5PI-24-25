import { Service, Inject } from 'typedi';

import { MedicalRecord } from '../domain/medicalRecord';
import { MedicalRecordId } from '../domain/medicalRecordId';
import { MedicalRecordMap } from '../mappers/MedicalRecordMap';

import { Document, FilterQuery, Model } from 'mongoose';
import { IMedicalRecordPersistence } from '../dataschema/IMedicalRecordPersistence';
import IMedicalRecordRepo from '../services/IRepos/IMedicalRecordRepo';

@Service()
export default class MedicalRecordRepo implements IMedicalRecordRepo {
    private models: any;

    constructor(
        @Inject('medicalRecordSchema') private medicalRecordSchema: Model<IMedicalRecordPersistence & Document>,
    ) { }
    
    
    public async findByPatientId(patientId: string): Promise<MedicalRecord | null> {
        const query = { patientId: patientId };
        const medicalRecordRecord = await this.medicalRecordSchema.findOne(query as FilterQuery<IMedicalRecordPersistence & Document>);
        if (medicalRecordRecord != null) {
            return MedicalRecordMap.toDomain(medicalRecordRecord);
        } else {
            return null;
        }
    }

    
    private createBaseQuery(): any {
        return {
            where: {},
        }
    }

    public async exists(medicalRecord: MedicalRecord): Promise<boolean> {
        const idX = medicalRecord.id instanceof MedicalRecordId ? (<MedicalRecordId>medicalRecord.id).toValue() : medicalRecord.id;

        const query = { id: idX };
        const medicalRecordDocument = await this.medicalRecordSchema.findOne(query as FilterQuery<IMedicalRecordPersistence & Document>);

        return !!medicalRecordDocument === true;
    }

    public async save(medicalRecord: MedicalRecord): Promise<MedicalRecord> {
        const query = { id: medicalRecord.id.toString() };

        const medicalRecordDocument = await this.medicalRecordSchema.findOne(query);



        try {
            if (medicalRecordDocument === null) {

                const rawMedicalRecord: any = MedicalRecordMap.toPersistence(medicalRecord);
                const medicalRecordCreated = await this.medicalRecordSchema.create(rawMedicalRecord);

                return MedicalRecordMap.toDomain(medicalRecordCreated);
            } else {
                medicalRecordDocument.id = medicalRecord.id;
                await medicalRecordDocument.save();
                return medicalRecord;
            }
        } catch (err) {
            throw err;
        }
    }

    public async findById(medicalRecordId: MedicalRecordId | string): Promise<MedicalRecord> {
        const query = { id: medicalRecordId };
        const medicalRecordRecord = await this.medicalRecordSchema.findOne(query as FilterQuery<IMedicalRecordPersistence & Document>);
        if (medicalRecordRecord != null)
            return MedicalRecordMap.toDomain(medicalRecordRecord);
        else
            return null;
    }


    public async findAll(): Promise<MedicalRecord[]> {
        const medicalRecordRecords = await this.medicalRecordSchema.find();
        return medicalRecordRecords.map((medicalRecordRecord) => MedicalRecordMap.toDomain(medicalRecordRecord));
    }

    public async removeAll(): Promise<void> {
        await this.medicalRecordSchema.deleteMany({});
    }



    public async updateMedicalRecord(medicalRecord: MedicalRecord): Promise<MedicalRecord> {
        const query = { id: medicalRecord.id.toString() };
        const medicalRecordDocument = await this.medicalRecordSchema.findOne(query);

        if (medicalRecordDocument != null) {
            medicalRecordDocument.set(MedicalRecordMap.toPersistence(medicalRecord));
            await medicalRecordDocument.save();
            return MedicalRecordMap.toDomain(medicalRecordDocument);
        } else {
            throw new Error('Medical record not found');
        }
    }

}