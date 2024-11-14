import { TestBed } from '@angular/core/testing';

import { OperationTypesService } from './operation-type.service';

describe('OperationTypeService', () => {
  let service: OperationTypesService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(OperationTypesService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
