import MedicalConditionService from '../../../src/services/medicalConditionService';
import { MedicalConditionCatalog } from '../../../src/domain/medicalConditionCatalog';
import MedicalConditionRepo from '../../../src/repos/medicalConditionRepo';

describe('MedicalConditionService', () => {
  let medicalConditionService: MedicalConditionService;
  let medicalConditionRepo: jest.Mocked<MedicalConditionRepo>;

  beforeEach(() => {
    medicalConditionRepo = {
      findByCode: jest.fn(),
      save: jest.fn(),
      findAll: jest.fn(),
      delete: jest.fn(),
    } as jest.Mocked<MedicalConditionRepo>;

    medicalConditionService = new MedicalConditionService(medicalConditionRepo);
  });

  it('should successfully retrieve a medical condition by code', async () => {
    const code = 'M01';
    const existingCondition = MedicalConditionCatalog.create({ id: '1', code, designation: 'Condition A' }).getValue();

    medicalConditionRepo.findByCode.mockResolvedValue(existingCondition);

    const result = await medicalConditionService.getMedicalCondition(code);

    expect(result.isSuccess).toBe(true);
    expect(result.getValue().code).toEqual(existingCondition.props.code);
  });

  it('should fail if medical condition is not found by code', async () => {
    const code = 'MC002';

    medicalConditionRepo.findByCode.mockResolvedValue(null);

    const result = await medicalConditionService.getMedicalCondition(code);

    expect(result.isFailure).toBe(true);
    expect(result.error).toEqual('No medical condition found');
  });

  it('should create a new medical condition', async () => {
    const newDTO = { code: 'MC003', designation: 'New Condition' };
    const newCondition = MedicalConditionCatalog.create(newDTO);

    medicalConditionRepo.save.mockResolvedValue(undefined);

    const result = await medicalConditionService.createMedicalCondition(newDTO);

    expect(result.isSuccess).toBe(false);
  });

  it('should fail to update a non-existing medical condition', async () => {
    const code = 'MC005';
    const updatedDTO = { code, designation: 'Non-Existent Condition' };

    medicalConditionRepo.findByCode.mockResolvedValue(null);

    const result = await medicalConditionService.updateMedicalCondition(code, updatedDTO);

    expect(result.isFailure).toBe(true);
    expect(result.error).toEqual('No medical condition found');
  });

  it('should delete an existing medical condition', async () => {
    const code = 'MC006';
    const existingCondition = MedicalConditionCatalog.create({ id: '1', code, designation: 'To Delete' });

    medicalConditionRepo.findByCode.mockResolvedValue(existingCondition);
    medicalConditionRepo.delete.mockResolvedValue(undefined);

    const result = await medicalConditionService.deleteMedicalCondition(code);

    expect(result.isSuccess).toBe(true);
  });

  it('should fail to delete a non-existing medical condition', async () => {
    const code = 'MC007';

    medicalConditionRepo.findByCode.mockResolvedValue(null);

    const result = await medicalConditionService.deleteMedicalCondition(code);

    expect(result.isFailure).toBe(true);
    expect(result.error).toEqual('No medical condition found');
  });

  

  it('should fail to list medical conditions if none exist', async () => {
    medicalConditionRepo.findAll.mockResolvedValue([]);

    const result = await medicalConditionService.listMedicalConditions();

    expect(result.isFailure).toBe(true);
    expect(result.error).toEqual('No medical conditions found');
  });
});