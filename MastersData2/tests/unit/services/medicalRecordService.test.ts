import { jest } from '@jest/globals';
import sinon from 'sinon';
import MedicalRecordService from '../../../src/services/medicalRecordService';
import IMedicalRecordRepo from '../../../src/services/IRepos/IMedicalRecordRepo';
import IAllergyCatalogRepo from '../../../src/services/IRepos/IAllergyCatalogRepo';
import IMedicalConditionRepo from '../../../src/services/IRepos/IMedicalConditionRepo';
import { Result } from '../../../src/core/logic/Result';
import { MedicalRecordMap } from '../../../src/mappers/MedicalRecordMap';
import { MedicalRecord } from '../../../src/domain/medicalRecord';
import { Allergy } from '../../../src/domain/allergy';
import { AllergyCatalogItem } from '../../../src/domain/allergyCatalogItem';
import { MedicalCondition } from '../../../src/domain/medicalCondition';
import MedicalRecordRepo from '../../../src/repos/medicalRecordRepo';
import AllergyCatalogRepo from '../../../src/repos/allergyCatalogRepo';
import MedicalConditionRepo from '../../../src/repos/medicalConditionRepo';
import { IAllergyDTO } from '../../../src/dto/IAllergyDTO';



describe('MedicalRecordService', () => {
    let medicalRecordRepo: sinon.SinonStubbedInstance<IMedicalRecordRepo>;
    let allergyCatalogRepo: sinon.SinonStubbedInstance<IAllergyCatalogRepo>;
    let medicalConditionRepo: sinon.SinonStubbedInstance<IMedicalConditionRepo>;
    let medicalRecordService: MedicalRecordService;

    beforeEach(() => {
        medicalRecordRepo = sinon.createStubInstance<IMedicalRecordRepo>(MedicalRecordRepo);
        allergyCatalogRepo = sinon.createStubInstance<IAllergyCatalogRepo>(AllergyCatalogRepo);
        medicalConditionRepo = sinon.createStubInstance<IMedicalConditionRepo>(MedicalConditionRepo);
    
        medicalRecordService = new MedicalRecordService(medicalRecordRepo, allergyCatalogRepo, medicalConditionRepo);
    });

    it('should search for a patient medical record by patient ID', async () => {
        const patientId = '123';
        const medicalRecordDTO = { patientId: '202410000099', allergies: [], medicalConditions: [] };
        const medicalRecord = MedicalRecord.create(medicalRecordDTO, [], []).getValue();
        medicalRecordRepo.findByPatientId.resolves(medicalRecord);

        const result = await medicalRecordService.searchMedicalRecordEntries(patientId, '');
    
        expect(result.isSuccess).toBe(true);
        expect(result.getValue()).toEqual(MedicalRecordMap.toDTO(medicalRecord));
    });

    it('should display the correct table for allergies when searched by name', async () => {
        const patientId = '202410000099';
        const name = 'Peanut';
        const allergy: IAllergyDTO = { id: '1', code: 'C01', designation: 'Peanut', description: 'Peanut Allergy' };

        const medicalRecordDTO = { patientId, allergies: [allergy], medicalConditions: [] };

        const medicalRecord = MedicalRecord.create(medicalRecordDTO, [], []).getValue();
        
        const allergyCatalogItem = AllergyCatalogItem.create({ id: '1', code: 'C01', designation: "Peanut Allergy" }).getValue();
        const allergyRecordDTO = { id: '1', allergyCatalogItem, date: new Date() };
        const allergyRecord = Allergy.create(allergyRecordDTO).getValue();
        medicalRecordRepo.findByPatientId.resolves(medicalRecord);
        allergyCatalogRepo.findById.resolves(allergyCatalogItem);

        const result = await medicalRecordService.searchMedicalRecordEntries(patientId, name);
        
        
        expect(result.isSuccess).toBe(true);
    });

    it('should display the correct table for medical conditions when searched by name', async () => {
        const patientId = '123';
        const name = 'Asthma';
        const medicalRecordDTO = { patientId, allergies: [], medicalConditions: ['1'] };
        const medicalRecord = MedicalRecord.create(medicalRecordDTO, [], []).getValue();
        const conditionRecord = MedicalCondition.create({ id: '1', name: 'Asthma' }).getValue();
        medicalRecordRepo.findByPatientId.resolves(medicalRecord);
        medicalConditionRepo.findByDomainId.resolves(conditionRecord);

        const result = await medicalRecordService.searchMedicalRecordEntries(patientId, name);

        expect(result.isSuccess).toBe(true);
    });

    it('should correctly filter search results based on input criteria', async () => {
        const patientId = '123';
        const name = 'Peanut';
        const medicalRecordDTO = { patientId, allergies: ['1', '2'], medicalConditions: ['1', '2'] };
        const medicalRecord = MedicalRecord.create(medicalRecordDTO, [], []).getValue();
        const allergyRecord1 = AllergyCatalogItem.create({ id: '1', name: 'Peanut Allergy' }).getValue();
        const allergyRecord2 = AllergyCatalogItem.create({ id: '2', name: 'Dust Allergy' }).getValue();
        const conditionRecord1 = MedicalCondition.create({ id: '1', name: 'Asthma' }).getValue();
        const conditionRecord2 = MedicalCondition.create({ id: '2', name: 'Peanut Condition' }).getValue();
        medicalRecordRepo.findByPatientId.resolves(medicalRecord);
        allergyCatalogRepo.findById.withArgs('1').resolves(allergyRecord1);
        allergyCatalogRepo.findById.withArgs('2').resolves(allergyRecord2);
        medicalConditionRepo.findByDomainId.withArgs('1').resolves(conditionRecord1);
        medicalConditionRepo.findByDomainId.withArgs('2').resolves(conditionRecord2);

        const result = await medicalRecordService.searchMedicalRecordEntries(patientId, name);

        expect(result.isSuccess).toBe(true);
    });
});