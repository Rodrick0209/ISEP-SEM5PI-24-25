import { MedicalRecord } from '../../../src/domain/MedicalRecord';
import { Allergy } from '../../../src/domain/allergy';
import { MedicalCondition } from '../../../src/domain/medicalCondition';
import { UniqueEntityID } from '../../../src/core/domain/UniqueEntityID';
import { Result } from '../../../src/core/logic/Result';
import { MedicalRecordId } from '../../../src/domain/medicalRecordId';

describe('MedicalRecord', () => {
  const mockAllergies = [new Allergy('Peanuts'), new Allergy('Dust')];
  const mockMedicalConditions = [new MedicalCondition('Asthma'), new MedicalCondition('Diabetes')];
  const patientId = 'patient-123';

  it('should create a valid medical record', () => {
    const result = MedicalRecord.create(patientId, mockAllergies, mockMedicalConditions);

    // Assert that the result is successful
    expect(result.isSuccess).toBe(true);
    const medicalRecord = result.getValue();
    
    // Assert that the patientId, allergies, and medical conditions are correct
    expect(medicalRecord.patientId).toBe(patientId);
    expect(medicalRecord.allergies).toEqual(mockAllergies);
    expect(medicalRecord.medicalConditions).toEqual(mockMedicalConditions);
  });

  it('should return correct patientId from getter', () => {
    const medicalRecord = new MedicalRecord({ patientId, allergies: mockAllergies, medicalConditions: mockMedicalConditions });

    expect(medicalRecord.patientId).toBe(patientId);
  });

  it('should return correct allergies from getter', () => {
    const medicalRecord = new MedicalRecord({ patientId, allergies: mockAllergies, medicalConditions: mockMedicalConditions });

    expect(medicalRecord.allergies).toEqual(mockAllergies);
  });

  it('should update allergies using setter', () => {
    const medicalRecord = new MedicalRecord({ patientId, allergies: mockAllergies, medicalConditions: mockMedicalConditions });

    const newAllergies = [new Allergy('Pollen')];
    medicalRecord.allergies = newAllergies;

    expect(medicalRecord.allergies).toEqual(newAllergies);
  });

  it('should return correct medicalConditions from getter', () => {
    const medicalRecord = new MedicalRecord({ patientId, allergies: mockAllergies, medicalConditions: mockMedicalConditions });

    expect(medicalRecord.medicalConditions).toEqual(mockMedicalConditions);
  });

  it('should update medicalConditions using setter', () => {
    const medicalRecord = new MedicalRecord({ patientId, allergies: mockAllergies, medicalConditions: mockMedicalConditions });

    const newConditions = [new MedicalCondition('Hypertension')];
    medicalRecord.medicalConditions = newConditions;

    expect(medicalRecord.medicalConditions).toEqual(newConditions);
  });

  it('should return correct UniqueEntityID', () => {
    const medicalRecord = new MedicalRecord({ patientId, allergies: mockAllergies, medicalConditions: mockMedicalConditions });
    const uniqueId = medicalRecord.id;
    
    expect(uniqueId).toBeInstanceOf(UniqueEntityID);
  });

  it('should return correct MedicalRecordId', () => {
    const medicalRecord = new MedicalRecord({ patientId, allergies: mockAllergies, medicalConditions: mockMedicalConditions });
    const medicalRecordId = medicalRecord.MedicalRecordId;

    expect(medicalRecordId).toBeInstanceOf(MedicalRecordId);
    expect(medicalRecordId.toValue()).toBe(medicalRecord.id.toValue());
  });
});