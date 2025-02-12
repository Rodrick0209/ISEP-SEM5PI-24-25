import { Service, Inject } from 'typedi';
import config from '../../config';
import IMedicalRecordDTO from '../dto/IMedicalRecordDTO';
import IMedicalRecordRepo from './IRepos/IMedicalRecordRepo';
import IMedicalRecordService from './IServices/IMedicalRecordService';
import { Result } from '../core/logic/Result';
import { MedicalRecord } from '../domain/medicalRecord';
import { MedicalRecordMap } from '../mappers/MedicalRecordMap';
import AllergyCatalogRepo from '../repos/allergyCatalogRepo';
import IAllergyCatalogRepo from './IRepos/IAllergyCatalogRepo';
import MedicalConditionRepo from '../repos/medicalConditionRepo';
import IMedicalConditionRepo from './IRepos/IMedicalConditionRepo';
import { AllergyId } from '../domain/allergyId';
import { Allergy } from '../domain/allergy';
import { MedicalConditionCatalog } from '../domain/medicalConditionCatalog';
import { cond, forEach } from 'lodash';
import mongoose from 'mongoose';
import { MedicalCondition } from '../domain/medicalCondition';

@Service()
export default class MedicalRecordService implements IMedicalRecordService {
    constructor(
        @Inject(config.repos.medicalRecord.name) private MedicalRecordRepo: IMedicalRecordRepo,
        @Inject(config.repos.allergyCatalog.name) private AllergyCatalogRepo: IAllergyCatalogRepo,
        @Inject(config.repos.medicalCondition.name) private MedicalConditionRepo: IMedicalConditionRepo,
    ) { }

    public async createMedicalRecord(MedicalRecordDTO: IMedicalRecordDTO): Promise<Result<IMedicalRecordDTO>> {


        try {
            let allergyIds: Allergy[] = [];

            for (const allergyName of MedicalRecordDTO.allergies) {
                const allergy = await this.AllergyCatalogRepo.findByCode(allergyName.code);
                if (allergy) {
                    const allergyResult = Allergy.create(allergy, allergyName.description);

                    if (allergyResult.isSuccess) {
                        allergyIds.push(allergyResult.getValue());
                    }
                }
            }

            let medicalConditionIds: MedicalCondition[] = [];

            for (const conditionName of MedicalRecordDTO.medicalConditions) {
                const condition = await this.MedicalConditionRepo.findByCode(conditionName.code);

                const conditionResult = MedicalCondition.create(condition, conditionName.date);

                if (conditionResult.isSuccess) {
                    medicalConditionIds.push(conditionResult.getValue());
                }
            }


            const MedicalRecordOrError = await MedicalRecord.create(MedicalRecordDTO.patientId, allergyIds, medicalConditionIds);



            if (MedicalRecordOrError.isFailure) {
                return Result.fail<IMedicalRecordDTO>(MedicalRecordOrError.errorValue());
            }

            const MedicalRecordResult = MedicalRecordOrError.getValue();

            await this.MedicalRecordRepo.save(MedicalRecordResult);


            const MedicalRecordDTOResult = MedicalRecordMap.toDTO(MedicalRecordResult) as IMedicalRecordDTO;
            return Result.ok<IMedicalRecordDTO>(MedicalRecordDTOResult);
        } catch (e) {
            throw e;
        }
    }

    public async listMedicalRecords(): Promise<Result<IMedicalRecordDTO[]>> {
        try {
            const allMedicalRecords = await this.MedicalRecordRepo.findAll();

            if (allMedicalRecords === null || allMedicalRecords.length === 0) {
                return Result.fail<IMedicalRecordDTO[]>('No medical records found');
            }

            const MedicalRecordsDTO = allMedicalRecords.map(rec => MedicalRecordMap.toDTO(rec) as IMedicalRecordDTO);

            return Result.ok<IMedicalRecordDTO[]>(MedicalRecordsDTO);
        } catch (e) {
            throw e;
        }
    }

    public async updateMedicalRecord(
        MedicalRecordDTO: IMedicalRecordDTO,
        id: string,
    ): Promise<Result<IMedicalRecordDTO>> {
        try {
            console.log("1OLA");

            const medicalRecord = await this.MedicalRecordRepo.findByPatientId(id.toString());
            
            if (medicalRecord === null) {
                return Result.fail<IMedicalRecordDTO>('Medical record not found');
            }
            console.log("OLA1");

            const allergyIds = [];
            for (const allergyName of MedicalRecordDTO.allergies) {
                const allergy = await this.AllergyCatalogRepo.findByCode(allergyName.code);
                if (allergy) {
                    const allergyResult = Allergy.create(allergy, allergyName.description, allergy.id);

                    if (allergyResult.isSuccess) {
                        allergyIds.push(allergyResult.getValue());
                    }
                }
            }
            console.log("OLA");

            const medicalConditionIds = [];
            for (const conditionName of MedicalRecordDTO.medicalConditions) {
                const condition = await this.MedicalConditionRepo.findByCode(conditionName.code);
                if (condition) {
                    const conditionResult = MedicalCondition.create(condition, conditionName.date, condition.id);

                    if (conditionResult.isSuccess) {
                        medicalConditionIds.push(conditionResult.getValue());
                    }
                }
            }

            medicalRecord.medicalConditions = medicalConditionIds;
            medicalRecord.allergies = allergyIds;

            await this.MedicalRecordRepo.updateMedicalRecord(medicalRecord);

            const MedicalRecordDTOResult = MedicalRecordMap.toDTO(medicalRecord) as IMedicalRecordDTO;
            return Result.ok<IMedicalRecordDTO>(MedicalRecordDTOResult);
        } catch (e) {
            throw e;
        }
    }

    public async searchMedicalRecordEntries(patientId: string, designation: string): Promise<Result<IMedicalRecordDTO>> {
        try {
            const medicalRecord = await this.MedicalRecordRepo.findByPatientId(patientId);

            if (!medicalRecord) {
                return Result.fail<IMedicalRecordDTO>('No medical record found for the given patient');
            }

            const allergyIds = [];
            for (const allergy of medicalRecord.allergies) {
                const allergyRecord = await this.AllergyCatalogRepo.findById(allergy);
                if (allergyRecord && allergyRecord.designation.includes(designation)) {
                    const allergyResult = Allergy.create(allergyRecord, allergy.description, allergyRecord.id);

                    if (allergyResult.isSuccess) {
                        allergyIds.push(allergyResult.getValue());
                    }
                }
            }

            const medicalConditionIds = [];
            for (const condition of medicalRecord.medicalConditions) {
                const conditionRecord = await this.MedicalConditionRepo.findByDomainId(condition);
                console.log(conditionRecord);
                if (conditionRecord && conditionRecord.designation.includes(designation)) {

                    const conditionResult = MedicalCondition.create(conditionRecord, condition.date, conditionRecord.id);

                    if (conditionResult.isSuccess) {
                        medicalConditionIds.push(conditionResult.getValue());
                    }
                }
            }

            medicalRecord.allergies = allergyIds;
            medicalRecord.medicalConditions = medicalConditionIds;

            const MedicalRecordDTO = MedicalRecordMap.toDTO(medicalRecord) as IMedicalRecordDTO;
            console.log(MedicalRecordDTO);
            return Result.ok<IMedicalRecordDTO>(MedicalRecordDTO);
        } catch (e) {
            throw e;
        }
    }

    public async getMedicalRecordByPatientId(patientId: string): Promise<Result<IMedicalRecordDTO>> {
        try {
            const medicalRecord = await this.MedicalRecordRepo.findByPatientId(patientId);

            if (!medicalRecord) {
                return Result.fail<IMedicalRecordDTO>('Medical record not found');
            }

            const MedicalRecordsDTO = MedicalRecordMap.toDTO(medicalRecord) as IMedicalRecordDTO;

            return Result.ok<IMedicalRecordDTO>(MedicalRecordsDTO);
        } catch (e) {
            throw e;
        }
    }
}
