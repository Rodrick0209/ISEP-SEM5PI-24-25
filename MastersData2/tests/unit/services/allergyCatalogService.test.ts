import AllergyCatalogService from '../../../src/services/allergyCatalogService';
import AllergyRepository from '../../../src/repos/allergyCatalogRepo';
import { AllergyCatalogItem } from '../../../src/domain/allergyCatalogItem';

describe('AllergyCatalogService', () => {
  let allergyCatalogService: AllergyCatalogService;
  let allergyRepo: jest.Mocked<AllergyRepository>;

  beforeEach(() => {
    // Mock the methods of AllergyRepository
    allergyRepo = {
      findByCode: jest.fn(),
      save: jest.fn(),
      findAll: jest.fn(),
      delete: jest.fn(),
    } as jest.Mocked<AllergyRepository>;

    allergyCatalogService = new AllergyCatalogService(allergyRepo);
  });

  it('should successfully update an allergy catalog item', async () => {
    const code = 'A001';
    const updatedDTO = { id: '7', code: 'A001', designation: 'Updated Allergy' };
    const existingItem = AllergyCatalogItem.create({ id: 'some-id', code, designation: 'Existing Allergy' }).getValue();

    // Mock the methods
    allergyRepo.findByCode.mockResolvedValue(existingItem);  // Use mockResolvedValue for promises
    allergyRepo.save.mockResolvedValue(undefined);

    const result = await allergyCatalogService.updateAllergyCatalogItem(code, updatedDTO);
    const resultItem = result;

    expect(result._value.id).toEqual(existingItem.id.toString());
    
  });

  it('should return failure if allergy is not found during update', async () => {
    const code = 'A002';
    const updatedDTO = { name: 'Updated Allergy' };

    // Mock the method to return null (item not found)
    allergyRepo.findByCode.mockResolvedValue(null);

    const result = await allergyCatalogService.updateAllergyCatalogItem(code, updatedDTO);

    expect(result.isFailure).toEqual(true);
  });

  it('should create a new allergy catalog item', async () => {
    const newDTO = { name: 'New Allergy' };
    const newItem = AllergyCatalogItem.create(newDTO).getValue();

    // Mock the save method
    allergyRepo.save.mockResolvedValue(undefined);

    const result = await allergyCatalogService.createAllergyCatalogItem(newDTO);

    
    expect(result).not.toBeNull();
  });

  it('should list all allergy catalog items', async () => {
    const allergyItems = [
      AllergyCatalogItem.create({ id: '1', code: 'A001', designation: 'Allergy 1' }).getValue(),
    ];

    // Mock the findAll method
    allergyRepo.findAll.mockResolvedValue(allergyItems);

    const result = await allergyCatalogService.listAllergiesCatalogItems();
    const resultItem = result._value;

    
    expect(resultItem.id).toEqual(allergyItems.id);
  });

  it('should delete an allergy catalog item', async () => {
    const code = 'A001';
    const existingItem = AllergyCatalogItem.create({ id: '1', code, designation: 'Allergy to delete' }).getValue();

    // Mock the methods
    allergyRepo.findByCode.mockResolvedValue(existingItem);
    allergyRepo.delete.mockResolvedValue(undefined);

    const result = await allergyCatalogService.deleteAllergyCatalogItem(code);

    expect(result.isSuccess).toEqual(true);
    
  });

  it('should return failure if allergy is not found during deletion', async () => {
    const code = 'A002';

    // Mock the method to return null (item not found)
    allergyRepo.findByCode.mockResolvedValue(null);

    const result = await allergyCatalogService.deleteAllergyCatalogItem(code);

    expect(result.isFailure).toEqual(true);
  });
});