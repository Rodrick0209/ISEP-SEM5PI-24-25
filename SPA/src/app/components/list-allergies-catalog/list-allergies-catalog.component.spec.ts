import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ListAllergiesCatalogComponent } from './list-allergies-catalog.component';
import { AllergyCatalogService } from '../../services/allergiesCatalog.service';
import { MessageService } from '../../services/message.service';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { RouterTestingModule } from '@angular/router/testing';
import { identity } from 'lodash';

describe('ListAllergiesCatalogComponent', () => {
  let component: ListAllergiesCatalogComponent;
  let fixture: ComponentFixture<ListAllergiesCatalogComponent>;
  let allergyCatalogService: jasmine.SpyObj<AllergyCatalogService>;
  let messageService: jasmine.SpyObj<MessageService>;
  let router: jasmine.SpyObj<Router>;

  beforeEach(async () => {
    const allergyCatalogServiceSpy = jasmine.createSpyObj('AllergyCatalogService', ['getAllergiesFromCatalog']);
    const messageServiceSpy = jasmine.createSpyObj('MessageService', ['getMessage']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      imports: [RouterTestingModule],
      providers: [
        { provide: AllergyCatalogService, useValue: allergyCatalogServiceSpy },
        { provide: MessageService, useValue: messageServiceSpy },
        { provide: Router, useValue: routerSpy }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(ListAllergiesCatalogComponent);
    component = fixture.componentInstance;
    allergyCatalogService = TestBed.inject(AllergyCatalogService) as jasmine.SpyObj<AllergyCatalogService>;
    messageService = TestBed.inject(MessageService) as jasmine.SpyObj<MessageService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize with success message and fetch allergies', () => {
    const mockAllergies = [{ id: 'A1', code: 'A1', designation: 'Peanuts', description: 'Allergy to peanuts' }];
    messageService.getMessage.and.returnValue('Success');
    allergyCatalogService.getAllergiesFromCatalog.and.returnValue(of(mockAllergies));

    component.ngOnInit();

    expect(component.successMessage).toBe('Success');
    expect(component.isLoading).toBeFalse();
    expect(component.allergiesCatalogItem).toEqual(mockAllergies);
  });

  it('should handle error when fetching allergies', () => {
    messageService.getMessage.and.returnValue('Success');
    allergyCatalogService.getAllergiesFromCatalog.and.returnValue(throwError('Error'));

    component.ngOnInit();

    expect(component.isLoading).toBeFalse();
    expect(component.allergiesCatalogItem).toEqual([]);
    expect(component.message).toBe('An error occurred while fetching allergies: ');
  });

  it('should navigate to add allergy page', () => {
    component.addAllergy();
    expect(router.navigate).toHaveBeenCalledWith(['/allergiesCatalog/add']);
  });

  it('should navigate to edit allergy page', () => {
    const mockAllergy = { id: 'A1', code: 'A1', designation: 'Peanuts', description: 'Allergy to peanuts' };
    component.editAllergy(mockAllergy);
    expect(router.navigate).toHaveBeenCalledWith(['/allergiesCatalog/edit', 'A1']);
  });
});