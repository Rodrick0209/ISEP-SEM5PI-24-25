import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { OperationTypesService, OperationType } from './operation-type.service';

describe('OperationTypesService', () => {
  let service: OperationTypesService;
  let httpMock: HttpTestingController;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [OperationTypesService]
    });
    service = TestBed.inject(OperationTypesService);
    httpMock = TestBed.inject(HttpTestingController);
  });

  afterEach(() => {
    httpMock.verify();
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should fetch operation types', () => {
    const dummyOperationTypes: OperationType[] = [
      { id: '1', name: 'Operation 1', status: 'active', specialization: 'spec1', preparationPhase: { duration: 1, requiredStaff: [] }, surgeryPhase: { duration: 2, requiredStaff: [] }, cleaningPhase: { duration: 3, requiredStaff: [] } }
    ];

    service.getOperationTypes().subscribe(operationTypes => {
      expect(operationTypes.length).toBe(1);
      expect(operationTypes).toEqual(dummyOperationTypes);
    });

    const req = httpMock.expectOne(service['apiUrl']);
    expect(req.request.method).toBe('GET');
    req.flush(dummyOperationTypes);
  });

  it('should filter operation types', () => {
    const dummyOperationTypes: OperationType[] = [
      { id: '1', name: 'Operation 1', status: 'active', specialization: 'spec1', preparationPhase: { duration: 1, requiredStaff: [] }, surgeryPhase: { duration: 2, requiredStaff: [] }, cleaningPhase: { duration: 3, requiredStaff: [] } }
    ];

    service.filterOperationTypes('Operation 1', 'active', 'spec1').subscribe(operationTypes => {
      expect(operationTypes.length).toBe(1);
      expect(operationTypes).toEqual(dummyOperationTypes);
    });

    const req = httpMock.expectOne(req => req.url === service['filterApiUrl'] && req.params.get('name') === 'Operation 1' && req.params.get('status') === 'active' && req.params.get('specialization') === 'spec1');
    expect(req.request.method).toBe('GET');
    req.flush(dummyOperationTypes);
  });

  it('should deactivate an operation type', () => {
    service.deactivateOperationType('1').subscribe(response => {
      expect(response).toBeUndefined();
    });

    const req = httpMock.expectOne(`${service['deactivateApiUrl']}/1`);
    expect(req.request.method).toBe('DELETE');
  });

  it('should get operation type by id', () => {
    const dummyOperationType: OperationType = { id: '1', name: 'Operation 1', status: 'active', specialization: 'spec1', preparationPhase: { duration: 1, requiredStaff: [] }, surgeryPhase: { duration: 2, requiredStaff: [] }, cleaningPhase: { duration: 3, requiredStaff: [] } };

    service.getOperationTypeById('1').subscribe(operationType => {
      expect(operationType).toEqual(dummyOperationType);
    });

    const req = httpMock.expectOne(`${service['getById']}/1`);
    expect(req.request.method).toBe('GET');
    req.flush(dummyOperationType);
  });

  it('should add a new operation type', () => {
    const newOperationType: OperationType = { id: '2', name: 'Operation 2', status: 'inactive', specialization: 'spec2', preparationPhase: { duration: 1, requiredStaff: [] }, surgeryPhase: { duration: 2, requiredStaff: [] }, cleaningPhase: { duration: 3, requiredStaff: [] } };

    service.addOperationType(newOperationType).subscribe(operationType => {
      expect(operationType).toEqual(newOperationType);
    });

    const req = httpMock.expectOne(service['createApiUrl']);
    expect(req.request.method).toBe('POST');
    req.flush(newOperationType);
  });

  it('should update an operation type', () => {
    const updatedOperationType: OperationType = { id: '1', name: 'Updated Operation', status: 'active', specialization: 'spec1', preparationPhase: { duration: 1, requiredStaff: [] }, surgeryPhase: { duration: 2, requiredStaff: [] }, cleaningPhase: { duration: 3, requiredStaff: [] } };

    service.updateOperationType(updatedOperationType).subscribe(operationType => {
      expect(operationType).toEqual(updatedOperationType);
    });

    const req = httpMock.expectOne(`${service['getById']}/1`);
    expect(req.request.method).toBe('PUT');
    req.flush(updatedOperationType);
  });
});