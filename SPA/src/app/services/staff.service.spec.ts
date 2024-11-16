import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';  // Adicionando o módulo de teste para o HttpClient
import { StaffService } from './staff.service';

describe('StaffService', () => {
  let service: StaffService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule]  // Aqui estamos importando o HttpClientTestingModule
    });
    service = TestBed.inject(StaffService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
